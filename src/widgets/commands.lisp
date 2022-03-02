(uiop:define-package #:hypernot/widgets/commands
  (:use #:cl)
  (:import-from #:hypernot/store)
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
  ((commands-widget :initarg :commands-widget
                    :type commands-widget
                    :reader commands-widget)))


(defwidget found-document-widget (command-widget)
  ((document :initarg :document
             :reader document)))

(defwidget commands-typeahead (typeahead-widget)
  ((parent :initform nil
           :type (or null commands-widget)
           :reader parent)
   (editor :initarg :editor
           :reader editor)))


(defwidget commands-widget (popup-widget)
  ((typeahead :initform (make-instance 'commands-typeahead)
              :reader typeahead)
   (editor :initarg :editor
           :reader editor)
   (current-node :initform nil
                 :accessor current-node)
   (current-cursor-position :initform nil
                            :accessor current-cursor-position)))


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


(defun show-commands (widget node cursor-position)
  (check-type widget commands-widget)
  (setf (current-node widget) node
        (current-cursor-position widget) cursor-position)
  (reblocks-ui/popup:show-popup widget))


(defmethod render-popup-content ((widget commands-widget))
  (reblocks/widget:render (typeahead widget)))


(defmethod print-object ((widget found-document-widget) stream)
  (print-unreadable-object (widget stream :type t)
    (format stream "~S"
            (common-doc:title (document widget)))))

(defmethod reblocks/widget:render ((widget found-document-widget))
  (reblocks/html:with-html
    (:p (format nil "Note ~S"
                (common-doc:title (document widget))))))


(defmethod execute-query ((widget commands-typeahead) query)
  (loop for document in (hypernot/store::search-notes query)
        collect (make-instance 'found-document-widget
                               :commands-widget (parent widget)
                               :document document)))


(defmethod process-typeahead-choice :after ((widget commands-typeahead) query selected-item-idx)
  (hide-popup (parent widget)))


(defmethod reblocks-typeahead::on-select ((widget found-document-widget))
  (let* ((commands-widget (commands-widget widget))
         (editor (editor commands-widget))
         (current-document (reblocks-text-editor/editor::document editor))
         (document-to-reference (document widget))
         (title (common-doc:title document-to-reference))
         (after-node (current-node commands-widget))
         (uri (format nil
                      "internal:~@[~A~]~@[#~A~]"
                      (common-doc:reference document-to-reference)
                      ;; section inside the document
                      nil))
         (new-node (common-doc:make-web-link uri
                                             (common-doc:make-text title))))
    (reblocks-text-editor/editor::insert-node current-document
                                              new-node
                                              after-node)))
