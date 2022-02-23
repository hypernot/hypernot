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


(defwidget autocomplete (reblocks-ui:ui-widget)
  ((visible :initform nil
            :accessor visible-p)))


(defun show (autocomplete)
  (setf (visible-p autocomplete) t)
  (reblocks/widget:update autocomplete)
  (reblocks/response:send-script
   (format nil "focusInAutocomplete(\"~A\")"
           (reblocks/widgets/dom:dom-id autocomplete))))

(defun hide (autocomplete)
  (setf (visible-p autocomplete) nil)
  (reblocks/widget:update autocomplete))


(defun process-autocomplete-choice (widget query)
  (log:info "User entered" query)
  (hide widget))


(defmethod reblocks/widget:render ((widget autocomplete))
  (reblocks-ui/form:with-html-form (:POST (lambda (&key query &allow-other-keys)
                                            (process-autocomplete-choice widget query))
                                    :class "popup")
    (:input :type "text"
            :name "query"
            :class "autocomplete-input"
            :autocomplete "off")
    (:ul (:li "First")
         (:li "Second"))))


(defmethod reblocks/widget:get-css-classes ((widget autocomplete))
  (append (when (visible-p widget)
            (list "active"))
          (call-next-method)))


(defmethod reblocks/dependencies:get-dependencies ((widget autocomplete))
  (list* (make-css)
         (make-js)
         (call-next-method)))
