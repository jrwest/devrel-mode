;; TODO: compile or force save to compile first (assuming edts mode)?
(defun devrel-mode-update-beam ()
  "updates the clusters lib dirs w/ beam file for current buffer"
  (interactive)
  (devrel-mode-update-beams (devrel-mode-buffer-beam-file-path) (devrel-mode-buffer-lib-dirs)
  (message "updated beams for %s" (file-name-nondirectory (buffer-file-name))))

(defun devrel-mode-update-beams (beam dirs)
  (if (null dirs) 'ok
    (progn
      (call-process "cp" nil (get-buffer-create "*devrel-mode*") nil beam (car dirs))
      (devrel-mode-update-beams beam (cdr dirs)))))

(defun devrel-mode-buffer-lib-dirs ()
  "returns list of lib directories for this riak dep. assumes ../riak"
  (file-expand-wildcards (devrel-mode-buffer-lib-wildcard)))

(defun devrel-mode-buffer-lib-wildcard ()
  "return wildcard string to search for lib directories for this riak dependency. assumes ../riak"
  (concat (devrel-mode-buffer-root-path)
          "../riak/dev/dev*/lib/"
          (devrel-mode-buffer-dep-name)
          "*/ebin"))

(defun devrel-mode-buffer-root-path ()
  "return directory below src directory this file is in"
  (substring (file-name-directory (buffer-file-name)) 0 -4))

(defun devrel-mode-buffer-dep-name ()
  "return which riak dependency the source file belongs to"
  (car (cdr (cdr (reverse (split-string (file-name-directory (buffer-file-name)) "/"))))))

(defun devrel-mode-buffer-beam-file-path ()
  "return path where beam file for this buffer should be located"
  (concat (file-name-directory (buffer-file-name))
          "../ebin/"
          (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
          ".beam"))

(defun devrel-mode-riak-console (riak-path node)
  "bin/riak console"
  (let* ((node-name (concat node "@127.0.0.1"))
        (path (concat riak-path "/dev/" node))) ;; TODO: deal w/ "/" (filename:join equiv?)    
    ;; TODO: turn off edts mode?
    (message "starting console for %s" node-name)
    (edts-shell-make-comint-buffer
     node-name node-name path
     (list "bin/riak" "console"))))

;;(defun devrel-mode-running-nodes (riak-path

;; TODO: bail on failure here? and reset nodes?
(defun devrel-mode-build-cluster (riak-path nodes)
  "build the cluster"
  (if (null nodes) 'ok
    (progn
      (devrel-mode-riak-start-nodes riak-path nodes)
      (devrel-mode-join-nodes riak-path (car nodes) (cdr nodes))
      (devrel-mode-riak-admin-cluster-plan riak-path (car nodes))
      (devrel-mode-riak-admin-cluster-commit riak-path (car nodes)))))

(defun devrel-mode-join-nodes (riak-path node nodes)
  "join nodes to node"
  (if (null nodes) 'ok
    (let ((join (concat node "@127.0.0.1")))
      (devrel-mode-riak-admin-cluster-join riak-path (car nodes) join)
      (devrel-mode-join-nodes riak-path node (cdr nodes)))))

(defun devrel-mode-riak-start-nodes (riak-path nodes)
  "start nodes"
  (if (null nodes) 'ok
    (if (= 0 (devrel-mode-riak-start riak-path (car nodes)))
        (devrel-mode-riak-start-nodes riak-path (cdr nodes))
      (message "failed to start nodes because %s failed to start" (car nodes)))))

;; TODO do good stuff now that we have blocking
(defun devrel-mode-riak-start (riak-path node)
  "bin/riak start"
  (message "starting %s" node)
  (devrel-mode-exit-message
   (devrel-mode-riak-cmd riak-path node "riak" '("start"))
   "started %s" (list node) "failed to start %s" (list node)))

(defun devrel-mode-riak-stop (riak-path node)
  "bin/riak stop"
  (message "stopping %s" node)
  (devrel-mode-exit-message
   (devrel-mode-riak-cmd riak-path node "riak" '("stop"))
   "stopped %s" (list node) "failed to stop %s" (list node)))

(defun devrel-mode-riak-ping (riak-path node)
  "bin/riak ping"
  (message "pinging %s" node)
  (devrel-mode-exit-message
    (devrel-mode-riak-cmd riak-path node "riak" '("ping"))
    "%s is running" (list node) "%s is NOT running" (list node)))

(defun devrel-mode-riak-admin-cluster-join (riak-path node join-to)
  "bin/riak-admin cluster join <join-to>"
  (message "join %s to %s" node join-to)
  (devrel-mode-riak-cmd riak-path node "riak-admin" (list "cluster" "join" join-to)))

(defun devrel-mode-riak-admin-cluster-plan (riak-path node)
  "bin/riak-admin cluster plan"
  (message "plan cluster on %s" node)
  (devrel-mode-riak-cmd riak-path node "riak-admin" '("cluster" "plan")))

(defun devrel-mode-riak-admin-cluster-commit (riak-path node)
  "bin/riak-admin cluster commit"
  (message "commit cluster on %s" node)
  (devrel-mode-riak-cmd riak-path node "riak-admin" '("cluster" "commit")))

(defun devrel-mode-riak-admin-member-status (riak-path node)
  "bin/riak-admin member-status"
  (devrel-mode-riak-cmd riak-path node "riak-admin" '("member-status")))

(defun devrel-mode-riak-cmd (riak-path node bin-file cmd)
  "runs command synchronously, dumping to output to the *devrel-mode* buf. exit status returned"
  (let* ((bin (concat "bin/" bin-file))
         (node-name (concat node "@127.0.0.1"))
         (dir-before (substring (pwd) 10))
         (path (concat riak-path "/dev/" node))
         (full-bin (concat path "/" bin))) ;; TODO: deal w/ "/" (filename:join equiv?)
    (apply #'call-process full-bin nil (get-buffer-create "*devrel-mode*") nil cmd)))

(defun devrel-mode-exit-message (result success-msg success-args failure-msg failure-args)
  "notifies result of process based on exit status"
  (progn
    (if (= result 0)
        (apply #'message success-msg success-args)
      (apply #'message failure-msg failure-args))
    result))


