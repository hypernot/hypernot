(uiop:define-package #:hypernot/widgets/commands
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks-ui/popup
                #:hide-popup
                #:render-popup-content
                #:popup-widget)
  (:import-from #:reblocks-typeahead
                #:focus-in
                #:process-typeahead-choice
                #:typeahead-widget
                #:execute-query))
(in-package #:hypernot/widgets/commands)


(defwidget command-widget ()
  ((editor :initarg :editor
           :reader editor)))


(defwidget found-document-widget (command-widget)
  ((document :initarg :document
             :reader document)))

(defwidget commands-typeahead (typeahead-widget)
  ((parent :initform nil
           :reader parent)
   (editor :initarg :editor
           :reader editor)))


(defwidget commands-widget (popup-widget)
  ((typeahead :initform (make-instance 'commands-typeahead)
              :reader typeahead)
   (editor :initarg :editor
           :reader editor)))


(defmethod initialize-instance :after ((widget commands-widget) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value (typeahead widget) 'parent)
        widget)
  (setf (slot-value (typeahead widget) 'editor)
        (editor widget)))


(defmethod reblocks-ui/popup:show-popup ((widget commands-widget))
  (call-next-method)

  (setf (reblocks-typeahead:results-items
         (reblocks-typeahead:typeahead-results
          (typeahead widget)))
        nil)

  (reblocks/widget:update widget)
  (focus-in (typeahead widget)))


(defmethod render-popup-content ((widget commands-widget))
  (reblocks/widget:render (typeahead widget)))


(defmethod print-object ((widget found-document-widget) stream)
  (print-unreadable-object (widget stream :type t)
    (format stream "~S"
            (document widget))))

(defmethod reblocks/widget:render ((widget found-document-widget))
  (reblocks/html:with-html
    (:p (format nil "Note ~S"
                (document widget)))))


(defmethod execute-query ((widget commands-typeahead) query)
  (let ((all-titles (uiop:symbol-call "HYPERNOT/WIDGETS/EDITOR"
                                      "ALL-DOCUMENT-TITLES")))
    (loop for item in all-titles
          when (str:containsp query item)
            collect (make-instance 'found-document-widget
                                   :editor (editor widget)
                                   :document item))))


(defmethod process-typeahead-choice :after ((widget commands-typeahead) query selected-item-idx)
  (hide-popup (parent widget)))
