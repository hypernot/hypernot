(uiop:define-package #:hypernot/store
  (:use #:cl)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:reblocks-text-editor/document/editable)
  (:import-from #:reblocks-text-editor/document/ops)
  (:import-from #:common-doc)
  (:import-from #:serapeum
                #:take))
(in-package #:hypernot/store)


(defparameter +documents-root+ #P"~/Documents/hypernot/")


(defun load-document (id-or-pathname)
  (let* ((pathname (etypecase id-or-pathname
                     (pathname id-or-pathname)
                     (string (uiop:merge-pathnames* (format nil "~A.scriba" id-or-pathname)
                                                    +documents-root+))))
         (id (etypecase id-or-pathname
               (pathname (pathname-name id-or-pathname))
               (string id-or-pathname)))
         (format (make-instance 'scriba:scriba))
         (full-doc (reblocks-text-editor/document/ops::add-missing-paragraphs
                    (common-doc.format:parse-document format pathname)))
         (editable-doc (make-instance 'reblocks-text-editor/document/editable::editable-document
                                      :title (common-doc:title full-doc)
                                      :reference id
                                      :children (common-doc:children full-doc))))
    (reblocks-text-editor/document/ops::add-reference-ids editable-doc)
    (values editable-doc)))


(defun search-notes (query &key (limit 10))
  (loop for pathname in (cl-fad:list-directory +documents-root+)
        for document = (load-document pathname)
        for title = (common-doc:title document) 
        while (< (length results)
                 limit)
        when (str:containsp (string-downcase query)
                            (string-downcase title))
          do (setf (common-doc:reference document)
                   (pathname-name pathname))
          and
            collect document into results
        finally (return (values (take limit results)
                                ;; has-more-p
                                (length= results (1+ limit))))))

