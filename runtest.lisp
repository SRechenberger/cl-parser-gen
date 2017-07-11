(pushnew (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)

(ql:quickload :cl-parser-gen-test)

(if (de.srechenberger.all-test:test-all)
  (exit :code 0)
  (exit :code 1))
