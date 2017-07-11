(pushnew (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)

(ql:quickload :cl-parser-gen-test)

(if (test:test-all)
  (uiop:quit 0)
  (uiop:quit 1))
