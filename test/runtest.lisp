(pushnew (truename ".") ql:*local-project-directories*)
(ql:register-local-projects)

(ql:quickload :cl-parser-gen-test)

(handler-case
    (if (test:test-all)
	(uiop:quit 0)
	(uiop:quit 1))
  (error (e)
    (format t "~a" e)
    (uiop:quit 2)))
