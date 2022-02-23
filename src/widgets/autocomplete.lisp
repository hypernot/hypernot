(uiop:define-package #:hypernot/widgets/autocomplete
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks-ui)
  (:import-from #:reblocks-ui/form)
  (:import-from #:reblocks/html
                #:with-html)
  (:import-from #:hypernot/widgets/autocomplete/css
                #:make-css)
  (:import-from #:hypernot/widgets/autocomplete/js
                #:make-js))
(in-package #:hypernot/widgets/autocomplete)


(defwidget results (reblocks-ui:ui-widget)
  ((items :initform nil
          :accessor results-items)))


(defwidget autocomplete (reblocks-ui:ui-widget)
  ((visible :initform nil
            :accessor visible-p)
   (results :initform (make-instance 'results)
            :reader autocompletion-results)))


(defun show (autocomplete)
  (setf (visible-p autocomplete) t)
  (setf (results-items (autocompletion-results autocomplete))
        nil)
  (reblocks/widget:update autocomplete)
  (reblocks/response:send-script
   (format nil "initAutocompletes(); focusInAutocomplete(\"~A\")"
           (reblocks/widgets/dom:dom-id autocomplete))))

(defun hide (autocomplete)
  (setf (visible-p autocomplete) nil)
  (reblocks/widget:update autocomplete))


(defun process-autocomplete-choice (widget query)
  (log:info "User entered" query)
  (hide widget))


(defgeneric update-results (widget query)
  (:method ((widget autocomplete) query)
    (log:info "Updating autocompletion results using \"~A\"" query)

    (setf (results-items (autocompletion-results widget))
          (execute-query widget query))
    (reblocks/widget:update (autocompletion-results widget))
    (values)))


(defgeneric execute-query (widget query)
  (:method ((widget autocomplete) query)
    ;; (loop for i from 0 upto 4
    ;;       collect (reblocks/widgets/string-widget:make-string-widget
    ;;                (format nil "~A ~A"
    ;;                        i
    ;;                        query)))
    ))


(defmethod reblocks/widget:render ((widget autocomplete))
  (flet ((on-update (&key query &allow-other-keys)
           (update-results widget query)))
    (let ((action-code (reblocks/actions:make-action #'on-update)))
      (reblocks-ui/form:with-html-form (:POST (lambda (&key query &allow-other-keys)
                                                (process-autocomplete-choice widget query))
                                        :class "popup")
        (:input :type "text"
                :name "query"
                :data-action-code action-code
                :class "autocomplete-input"
                :autocomplete "off")

        (reblocks/widget:render
         (autocompletion-results widget))))))


(defmethod reblocks/widget:render ((widget results))
  (let ((items (results-items widget)))
    (reblocks/html:with-html
      (cond
        (items
         (:ul
          (loop for item in items
                for idx upfrom 0
                do (:li (:input :type "radio"
                                :name "result"
                                :id (format nil "choice-~A" idx)
                                :onchange "this.form.onsubmit()"
                                :value idx)
                        (:label :for (format nil "choice-~A" idx)
                                (reblocks/widget:render item))))))
        (t
         (:p "No results."))))))


(defmethod reblocks/widget:get-css-classes ((widget autocomplete))
  (append (when (visible-p widget)
            (list "active"))
          (call-next-method)))


(defmethod reblocks/dependencies:get-dependencies ((widget autocomplete))
  (list* (make-css)
         (make-js)
         (call-next-method)))
