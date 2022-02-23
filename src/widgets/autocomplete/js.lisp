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

    (defun select-previous-item (autocomplete)
      (select-next-or-previous-item autocomplete nil))

    (defun select-next-item (autocomplete)
      (select-next-or-previous-item autocomplete t))
    
    (defun select-next-or-previous-item (autocomplete down-p)
      (chain console
             (log "Selecting next item" autocomplete))
      (let ((current-item (chain autocomplete
                                 (query-selector ".results > ul > li.active"))))
        (flet ((select (item)
                 (when item
                   (chain item
                          class-list
                          (add "active"))
                   (let ((input (chain item
                                       (query-selector "input"))))
                     (setf (@ input checked) t))))
               (deselect (item)
                 (when item
                   (chain item
                          class-list
                          (remove "active"))
                   (let ((input (chain item
                                       (query-selector "input"))))
                     (setf (@ input checked) nil)))))
          (cond
            (current-item
             (let ((next-item (if down-p
                                  (@ current-item next-sibling)
                                  (@ current-item previous-sibling))))
               (when (and next-item
                          (equal (@ next-item tag-name)
                                 "LI"))
                 (deselect current-item)
                 (select next-item))))
            (t
             (let ((first-item (chain autocomplete
                                      (query-selector ".results > ul > li"))))
               (select first-item)))))))

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
                                       (schedule-update event)))))
        (chain autocomplete
               (add-event-listener "keydown"
                                   (lambda (event)
                                     ;; (chain console (log "KEY CODE" (@ event target)))
                                     (when (or (eql (@ event target)
                                                    autocomplete)
                                               (eql (@ event target)
                                                    input))
                                       (let ((key-code (@ event key-code)))
                                         ;; (chain console (log "KEY CODE" key-code))
                                         (case key-code
                                           (38
                                            (select-previous-item autocomplete)
                                            (chain event (prevent-default)))
                                           (40
                                            (select-next-item autocomplete)
                                            (chain event (prevent-default))))))))))
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
