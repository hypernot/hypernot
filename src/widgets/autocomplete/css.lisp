(uiop:define-package #:hypernot/widgets/autocomplete/css
  (:use #:cl)
  (:import-from #:reblocks-lass))
(in-package #:hypernot/widgets/autocomplete/css)


(defun make-css ()
  (reblocks-lass:make-dependency
    '(body
      (.autocomplete
       :position fixed
       :top 0
       :left 0
       :width 100%
       :height 100vh
       :background "rgba(0,0,0,0.5)"
       :opacity 0
       :pointer-events none
       :transition 0.5s all

       (.popup
        :position absolute
        :top 50%
        :left 50%
        ;; Центрируем и масштабируем в 0 само окно
        :transform "translate(-50%, -50%) scale(0)" 
        :background "#fff"
        :width 400px
        :padding 25px
        :transition 0.5s all))
      ((:and .autocomplete .active)
       :opacity 1
       :pointer-events all
       :transition 0.5s all

       (.popup
        ;; Так же центрируем и плавно увеличиваем
        :transform "translate(-50%, -50%) scale(1)"
        :transition 0.5s all)))))
