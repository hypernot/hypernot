(uiop:define-package #:hypernot/widgets/editor
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks-text-editor/editor))
(in-package #:hypernot/widgets/editor)


(defparameter +default-title+ "Untitled")


(defwidget editor (reblocks-ui:ui-widget reblocks-text-editor/editor::editor)
  ((title :type string
          :initform +default-title+
          :accessor document-title)))



(defmethod reblocks/widget:render ((widget editor))
  (labels ((reset-text (&rest args)
             (declare (ignore args))
             (setf (slot-value widget 'reblocks-text-editor/editor::document)
                   (reblocks-text-editor/editor::make-initial-document)
                   (document-title widget)
                   +default-title+)
             (reblocks/widget:update widget))
           (update-title (&key text &allow-other-keys)
             (log:info "Updating document title to" text)
             (setf (document-title widget) text)))
    
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
