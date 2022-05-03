(uiop:define-package #:hypernot/widgets/editor
  (:use #:cl)
  (:import-from #:uuid)
  (:import-from #:reblocks-ui/popup)
  (:import-from #:trivial-timers)
  (:import-from #:scriba)
  (:import-from #:reblocks-ui)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:hypernot/store)
  (:import-from #:reblocks-text-editor/editor)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:parenscript
                #:chain
                #:@
                #:regex)
  (:import-from #:hypernot/widgets/commands
                #:commands-widget)
  (:import-from #:hypernot/widgets/search
                #:search-typeahead))
(in-package #:hypernot/widgets/editor)


(defparameter +default-title+ "Untitled")


(defun make-document-id ()
  (uuid:print-bytes nil (uuid:make-v4-uuid)))


(defwidget debug-widget ()
  ((editor :initform nil
           :initarg :editor
           :reader editor)))


(defwidget editor (reblocks-ui:ui-widget reblocks-text-editor/editor::editor)
  ((id :type string
       :initform (make-document-id)
       :accessor document-id)
   (title :type string
          :initform +default-title+
          :accessor document-title)
   (save-timer :type (or null trivial-timers:timer)
               :initform nil
               :accessor save-timer)
   (save-thread :type (or null bt:thread)
                :initform nil
                :accessor save-thread
                :documentation "We have to use separate thread for running our timers,
                                because Hunchentoot's thread will die after each web request.")
   (search-widget :initform nil
                  :reader search-widget)
   (commands-widget :initform nil
                    :reader commands-widget)
   (debug-widget :initform nil
                 :reader debug-widget)))


(defmethod initialize-instance :after ((widget editor) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value widget 'commands-widget)
        (make-instance 'commands-widget
                       :editor widget))
  (setf (slot-value widget 'search-widget)
        (make-instance 'search-typeahead
                       :editor widget))
  (setf (slot-value widget 'debug-widget)
        (make-instance 'debug-widget
                       :editor widget)))


(defun ensure-save-timer-created (widget)
  (let ((thread (save-thread widget)))
    (when (or (null thread)
              (not (bt:thread-alive-p thread)))
      (setf thread
            (bt:make-thread (lambda ()
                              (loop do (sleep 60)))
                            :name "Thread to save document"))
      
      (setf (save-timer widget)
            (trivial-timers:make-timer
             (lambda ()
               (save-document widget))
             :name "Save document timer"
             :thread thread)
            (save-thread widget)
            thread))))


(defun get-document-path (widget)
  (merge-pathnames (format nil "~A.scriba"
                           (document-id widget))
                   hypernot/store::+documents-root+))


(defun save-document (widget)
  (log:info "Saving document")
  (let ((path (get-document-path widget))
        ;; We need this to prevent Markdown markup leaking to the
        ;; document on disk.
        (reblocks-text-editor/html::*hide-markup-nodes* t)
        (document (common-doc:make-document (document-title widget)
                                            :children (reblocks-text-editor/editor::document widget))))
    (ensure-directories-exist path)
    (with-open-file (s path
                       :direction :output
                       :if-exists :supersede)
      (scriba.emitter:emit document
                           s))))


(defun reset (widget)
  (setf (slot-value widget 'reblocks-text-editor/editor::document)
        (reblocks-text-editor/editor::make-initial-document)
        (document-title widget)
        +default-title+
        (document-id widget)
        (make-document-id)))


(defun schedule-autosave (widget)
  (log:info "Scheduling document saving in 5 seconds")
  (ensure-save-timer-created widget)
  (trivial-timers:schedule-timer (save-timer widget) 5))



(defmethod reblocks-text-editor/editor:on-document-update ((widget editor))
  (call-next-method)
  (schedule-autosave widget)
  (reblocks/widget:update (debug-widget widget)))


;; (defmethod reblocks/widget:update :after ((widget editor) &key &allow-other-keys)
;;   (log:info "Scheduling document saving in 5 seconds")
;;   (trivial-timers:schedule-timer (save-timer widget) 5))


(defmethod reblocks/widget:render ((widget editor))
  (labels ((reset-text (&rest args)
             (declare (ignore args))
             (reset widget)
             (reblocks/widget:update widget)
             (select-title widget))
           (select-title (widget)
             ;; We need this JS code to focus on a new note's title
             ;; and prepare it for entering a new text:
             (let ((selector (format nil "#~A .document-title"
                                     (reblocks/widgets/dom:dom-id widget))))
               (reblocks/response:send-script
                `(let ((title (chain document
                                     (query-selector ,selector))))
                   (chain title
                          (focus))
                   (chain window
                          (get-selection)
                          (select-all-children title))))))
           (update-title (&key text &allow-other-keys)
             (log:info "Updating document title to" text)
             (setf (document-title widget) text)
             (schedule-autosave widget)))
    
    (let* ((action-code (reblocks/actions:make-action #'update-title))
           (update-title-action
             (format nil "initiateAction(\"~A\", {\"args\": {\"text\": this.innerText}}); return false;"
                     action-code)))

      (reblocks/widget:render
       (commands-widget widget))
      
      (reblocks/widget:render
       (search-widget widget))
      (reblocks/html:with-html
        (:h1 :contenteditable t
             :class "document-title"
             :style "outline: none"
             :onblur update-title-action
             :title (format nil "~A.scriba"
                            (document-id widget))
             (document-title widget))
        (call-next-method)

        (:p (:button :class "button"
                     :onclick (reblocks/actions:make-js-action #'reset-text)
                     "New")))

      ;; TODO: remove after lisp restart
      ;; (unless (debug-widget widget)
      ;;   (setf (slot-value widget 'debug-widget)
      ;;         (make-instance 'debug-widget
      ;;                        :editor widget)))
      
      (reblocks/widget:render
       (debug-widget widget)))))


(defun make-zero-spaces-visible (text)
  (str:replace-all reblocks-text-editor/utils/text::+zero-width-space+
                   "&ZeroWidthSpace;"
                   text))


(defgeneric render-common-doc-tree (node)
  (:method ((node common-doc:content-node))
    (with-html
      (:dl
       (:dt (format nil "~S (~A)"
                    (class-name (class-of node))
                    (common-doc:reference node)))
       (:dd (mapc #'render-common-doc-tree
                  (common-doc:children node))))))
  (:method ((node common-doc:text-node))
    (with-html
      (:p (:b (format nil "TEXT (~A): "
                      (common-doc:reference node)))
          (:span (format nil "\"~A\""
                         (make-zero-spaces-visible
                          (common-doc:text node)))))))
  (:method ((node common-doc:image))
    (with-html
      (:p (:b (format nil "IMAGE (~A): "
                      (common-doc:reference node)))
          (common-doc:source node)))))


(defmethod reblocks/widget:render ((widget debug-widget))
  (let* ((document (reblocks-text-editor/editor::document
                    (editor widget)))
         (left (reblocks-text-editor/document/editable::text-before-caret document))
         (right (reblocks-text-editor/document/editable::text-after-caret document))
         (line (concatenate 'string
                            left
                            "❙"
                            right)))
    (reblocks/html:with-html
      (:label "Text around caret:")
      (:div :class "inner"
            (:pre (:code line)))
      
      (:label "Document tree:")
      (:div :class "document-tree"
            (render-common-doc-tree document)))))


(defmethod reblocks-text-editor/editor::on-shortcut ((widget editor) key-code node cursor-position)
  (hypernot/widgets/commands::show-commands (commands-widget widget)
                                            node
                                            cursor-position))


(defun make-css-code ()
  (reblocks-lass:make-dependency
    '(body
      (.document-tree
       (dd
        :padding-left 2rem))
      (.debug-widget
       (.inner
        :border 1px solid "#555"
        :margin-left -0.5em
        :padding 0.5em
        (p
         :margin 0
         (code
          :white-space pre))
        (.caret
         :width 0.3em
         :height 0.8em
         :background-color "#777"
         :display inline-block
         :margin-left 0.1em
         :margin-right 0.1em)))
      (.editor
       :margin-left auto
       :margin-right auto
       :width 80%))))


(defun make-js-code ()
  (reblocks-parenscript:make-dependency
    (progn
      (defun initialize-document-title (title-node)
        (chain this
               (add-event-listener
                "keydown"
                (lambda (event)
                  (when (= (@ event target)
                           title-node)
                    (when (= (@ event key-code)
                             13)
                      (chain event
                             (prevent-default)))))))
        (chain this
               (add-event-listener
                "paste"
                (lambda (event)
                  (let* ((text (chain (or (@ event clipboard-data)
                                          (@ window clipboard-data))
                                      (get-data "text")
                                      (replace (regex "/\\n/g")
                                               " "))))
                    (let ((sel (chain window
                                      (get-selection))))
                      (when (> (@ sel range-count) 0)
                        (chain sel
                               (delete-from-document))
                        (chain sel
                               (get-range-at 0)
                               (insert-node
                                (chain document
                                       (create-text-node text)))))))
                  (chain event
                         (prevent-default))))))

      (chain (j-query document)
             (ready (lambda ()
                      (chain document
                             (query-selector-all ".editor .document-title")
                             (for-each initialize-document-title))))))))


(defmethod reblocks/dependencies:get-dependencies ((widget editor))
  (list* (make-css-code)
         (make-js-code)
         (call-next-method)))



(defmethod reblocks-text-editor/editor::process-link ((widget editor) &key href &allow-other-keys)
  (log:debug "Opening URL" href)
  (cond
    ((str:starts-with-p "internal:" href)
     (let* ((uri (quri:uri href))
            (document-id (quri:uri-path uri)))
       (unless document-id
         (error "Document with URI ~A not found."
                href))

       ;; First of all, we need to save the current document
       (save-document widget)
       
       (let ((document (hypernot/store::load-document document-id)))
         (setf (reblocks-text-editor/editor::document widget)
               document
               ;; TODO: use original title and reference from the document
               ;; when editing the title and saving the document.
               (document-title widget)
               (common-doc:title document)
               (document-id widget)
               (common-doc:reference document)))
       (reblocks/widget:update widget)))))
