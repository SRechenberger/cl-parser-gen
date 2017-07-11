(pushnew (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)

(ql:quickload :cl-parser-gen-test)

(if (de.srechenberger.all-test:test-all)
  (uiop:quit 0)
  (uiop:quit 1))
