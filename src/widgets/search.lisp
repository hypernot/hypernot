(uiop:define-package #:hypernot/widgets/search
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
(in-package #:hypernot/widgets/search)


(defwidget found-document-widget ()
  ((document-title :initarg :title
                   :reader document-title)
   (document-id :initarg :id
                   :reader document-id)
   (typeahead-widget :initarg :parent
                     :reader parent)))


(defwidget search-typeahead (typeahead-widget)
  ((editor :initarg :editor
           :reader editor))
  (:default-initargs
   :placeholder "Type something to search notes..."))


(defmethod print-object ((widget found-document-widget) stream)
  (print-unreadable-object (widget stream :type t)
    (format stream "~S"
            (document-title widget))))


(defmethod reblocks/widget:render ((widget found-document-widget))
  (reblocks/html:with-html
    (:p (document-title widget))))


(defmethod execute-query ((widget search-typeahead) query)
  (loop for document in (hypernot/store::search-notes query)
        collect (make-instance 'found-document-widget
                               :parent widget
                               :id (common-doc:reference document)
                               :title (common-doc:title document))))


(defmethod process-typeahead-choice :after ((widget search-typeahead) query selected-item-idx)
  (reblocks-typeahead::hide-results widget))


(defmethod reblocks-typeahead::on-select ((widget found-document-widget))
  (let* ((typeahead-widget (parent widget))
         (editor (editor typeahead-widget)))
    (reblocks-text-editor/editor::process-link editor
                                               :href (format nil "internal:~A"
                                                             (document-id widget)))))


(defmethod reblocks/dependencies:get-dependencies ((widget search-typeahead))
  (list* 
   (reblocks-lass:make-dependency
     '(.search-typeahead
       :margin-top 1em))
   (call-next-method)))
