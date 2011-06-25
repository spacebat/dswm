(asdf:operate 'asdf:load-op 'awesome-mode-line)

(proclaim '(optimize (safety 1) (speed 0) (space 0) (debug 3)))
(awesome-mode-line:show-awesome-mode-line)
