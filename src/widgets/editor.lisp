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
                #:chain)
  (:import-from #:hypernot/widgets/commands
                #:commands-widget)
  (:import-from #:hypernot/widgets/search
                #:search-typeahead))
(in-package #:hypernot/widgets/editor)


(defparameter +default-title+ "Untitled")


(defun make-document-id ()
  (uuid:print-bytes nil (uuid:make-v4-uuid)))


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
                    :reader commands-widget)))


(defmethod initialize-instance :after ((widget editor) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value widget 'commands-widget)
        (make-instance 'commands-widget
                       :editor widget))
  (setf (slot-value widget 'search-widget)
        (make-instance 'search-typeahead
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
        (reblocks-text-editor/html::*render-markup* nil)
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
  (schedule-autosave widget))


;; (defmethod reblocks/widget:update :after ((widget editor) &key &allow-other-keys)
;;   (log:info "Scheduling document saving in 5 seconds")
;;   (trivial-timers:schedule-timer (save-timer widget) 5))


(defmethod reblocks/widget:render ((widget editor))
  (labels ((reset-text (&rest args)
             (declare (ignore args))
             (reset widget)
             (reblocks/widget:update widget))
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
             :style "outline: none"
             :onblur update-title-action
             (document-title widget))
        (:h5 (document-id widget))
        (call-next-method)

        (:p (:button :class "button"
                     :onclick (reblocks/actions:make-js-action #'reset-text)
                     "New"))))))


(defmethod reblocks-text-editor/editor::on-shortcut ((widget editor) key-code node cursor-position)
  (hypernot/widgets/commands::show-commands (commands-widget widget)
                                            node
                                            cursor-position))


(defun make-css-code ()
  (reblocks-lass:make-dependency
    '(body
      (.editor
       :margin-left auto
       :margin-right auto
       :width 80%))))


(defmethod reblocks/dependencies:get-dependencies ((widget editor))
  (list* (make-css-code)
         (call-next-method)))



(defmethod reblocks-text-editor/editor::process-link ((widget editor) &key href &allow-other-keys)
  (cond
    ((str:starts-with-p "internal:" href)
     (let ((document-id (second (str:split #\: href))))
       (multiple-value-bind (document title id)
           (hypernot/store::load-document document-id)
         (setf (reblocks-text-editor/editor::document widget)
               document
               (document-title widget)
               title
               (document-id widget)
               id))
       (reblocks/widget:update widget)))))
