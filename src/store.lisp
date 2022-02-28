(uiop:define-package #:hypernot/store
  (:use #:cl)
  (:import-from #:alexandria
                #:length=)
  (:import-from #:serapeum
                #:take))
(in-package #:hypernot/store)


(defparameter +documents-root+ #P"~/Documents/hypernot/")


(defun search-notes (query &key (limit 10))
  (loop for pathname in (cl-fad:list-directory +documents-root+)
        for document = (common-doc.format:parse-document (make-instance 'scriba:scriba)
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

