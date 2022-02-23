(uiop:define-package #:hypernot/widgets/autocomplete/js
  (:use #:cl)
  (:import-from #:reblocks-parenscript)
  (:import-from #:parenscript
                #:@
                #:chain))
(in-package #:hypernot/widgets/autocomplete/js)


(defun make-js ()
  (reblocks-parenscript:make-dependency

    (defun focus-in-autocomplete (element-id)
      (chain console
             (log "Focusing in autocomplete"))
      (let* ((autocomplete (chain document
                                  (get-element-by-id element-id)))
             (input (chain autocomplete
                           (query-selector ".autocomplete-input"))))
        (chain input
               (focus))))

    (defun init-autocomplete (node)
      (chain node
             (add-event-listener
              "click"
              (lambda (event)
                (when (eql (ps:@ event target)
                           node)
                  (chain event
                         (prevent-default))
                  (chain node
                         class-list
                         (remove "active")))))))

    (defun init-autocompletes ()
      (chain document
             (query-selector-all ".autocomplete")
             (for-each init-autocomplete)))
    
    (chain window
           (add-event-listener
            "load"
            init-autocompletes))))
