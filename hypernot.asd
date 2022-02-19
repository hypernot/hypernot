(pushnew "~/projects/lisp/reblocks/" asdf:*central-registry*
         :test #'equal)
(pushnew "~/projects/lisp/reblocks-ui/" asdf:*central-registry*
         :test #'equal)
(pushnew "~/projects/lisp/zibaldone/" asdf:*central-registry*
         :test #'equal)
(pushnew "~/projects/lisp/scriba/" asdf:*central-registry*
         :test #'equal)
(pushnew "~/projects/lisp/commondoc-markdown/" asdf:*central-registry*
         :test #'equal)

(defsystem "hypernot"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("hypernot/widgets/editor"))
