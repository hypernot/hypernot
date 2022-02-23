(uiop:define-package #:hypernot/widgets/autocomplete/js
  (:use #:cl)
  (:import-from #:reblocks-parenscript)
  (:import-from #:parenscript
                #:create
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

    (defun update-autocompletion-results (autocompletion-input query)
      (initiate-action (@ autocompletion-input dataset action-code)
                       (create :args (create :query query))))
    
    (defun schedule-update (event)
      (let* ((autocompletion-input (@ event target))
             (timer-id (@ autocompletion-input timer-id)))
        (when timer-id
          (clear-timeout timer-id))
        (setf (@ autocompletion-input timer-id)
              (set-timeout (lambda ()
                             (update-autocompletion-results
                              autocompletion-input
                              (@ autocompletion-input value)))
                           250))))

    (defun init-autocomplete (autocomplete)
      (chain console
             (log "Initializing autocomplete"
                  autocomplete))
      (let ((input (chain autocomplete
                          (query-selector ".autocomplete-input"))))
        (chain input
               (add-event-listener "input"
                                   (lambda (event)
                                     (when (eql (@ event target)
                                                input)
                                       (schedule-update event))))))
      ;; (chain node
      ;;        (add-event-listener
      ;;         "click"
      ;;         (lambda (event)
      ;;           (when (eql (ps:@ event target)
      ;;                      node)
      ;;             (chain event
      ;;                    (prevent-default))
      ;;             (chain node
      ;;                    class-list
      ;;                    (remove "active"))))))
      
      )

    (defun init-autocompletes ()
      (chain document
             (query-selector-all ".autocomplete")
             (for-each init-autocomplete)))
    
    (chain window
           (add-event-listener
            "load"
            init-autocompletes))))
