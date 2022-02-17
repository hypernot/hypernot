(uiop:define-package #:hypernot/widgets/editor
  (:use #:cl)
  (:import-from #:uuid)
  (:import-from #:trivial-timers)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks-text-editor/editor))
(in-package #:hypernot/widgets/editor)


(defparameter +default-title+ "Untitled")

(defparameter +documents-root+ #P"~/Documents/hypernot/")


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
                                because Hunchentoot's thread will die after each web request.")))


(defun make-new-save-timer (widget)
  (let ((thread (or (save-thread widget)
                    (bt:make-thread (lambda ()
                                      (loop do (sleep 60)))
                                    :name "Thread to save document"))))
    (setf (save-timer widget)
          (trivial-timers:make-timer
           (lambda ()
             (save-document widget))
           :name "Save document timer"
           :thread thread)
          (save-thread widget)
          thread)))


(defmethod initialize-instance :after ((widget editor) &rest initargs)
  (declare (ignore initargs))
  (make-new-save-timer widget))


(defun get-document-path (widget)
  (merge-pathnames (format nil "~A.scriba"
                           (document-id widget))
                   +documents-root+))


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
        (make-document-id))
  ;; TODO: temporary, for upgrade
  (make-new-save-timer widget))



(defmethod reblocks-text-editor/editor:on-document-update ((widget editor))
  (call-next-method)
  (log:info "Scheduling document saving in 5 seconds")
  (trivial-timers:schedule-timer (save-timer widget) 5))


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
             (trivial-timers:schedule-timer (save-timer widget) 5)))
    
    (let* ((action-code (reblocks/actions:make-action #'update-title))
           (update-title-action
             (format nil "initiateAction(\"~A\", {\"args\": {\"text\": this.innerText}}); return false;"
                     action-code)))
      
      (reblocks/html:with-html
        (:h1 :contenteditable t
             :style "outline: none"
             :onblur update-title-action
             (document-title widget))
        (call-next-method)

        (:p (:button :class "button"
                     :onclick (reblocks/actions:make-js-action #'reset-text)
                     "New"))))))


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
