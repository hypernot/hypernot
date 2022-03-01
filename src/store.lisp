(uiop:define-package #:hypernot/store
  (:use #:cl)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:reblocks-text-editor/document/editable)
  (:import-from #:common-doc)
  (:import-from #:serapeum
                #:take))
(in-package #:hypernot/store)


(defparameter +documents-root+ #P"~/Documents/hypernot/")


(defun load-document (id)
  (let* ((pathname (uiop:merge-pathnames* (format nil "~A.scriba" id)
                                          +documents-root+))
         (format (make-instance 'scriba:scriba))
         (full-doc (common-doc.format:parse-document format pathname))
         (editable-doc (make-instance 'reblocks-text-editor/document/editable::editable-document
                                      :children (common-doc:children full-doc))))
    (reblocks-text-editor/document/ops::add-reference-ids editable-doc)
    (values editable-doc
            (common-doc:title full-doc)
            id)))


(defun search-notes (query &key (limit 10))
  (loop with format = (make-instance 'scriba:scriba)
        for pathname in (cl-fad:list-directory +documents-root+)
        for document = (common-doc.format:parse-document format
                                                         pathname)
        for idx upto limit
        for title = (common-doc:title document)
        when (str:containsp (string-downcase query)
                            (string-downcase title))
          do (setf (common-doc:reference document)
                   (pathname-name pathname))
          and
            collect document into results
        finally (return (values (take limit results)
                                ;; has-more-p
                                (length= results (1+ limit))))))

