;; Mode Globals and Definitions
(defvar devrel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-r\C-b" 'devrel-mode-update-beam)
    (define-key map "\C-x\C-r\C-a" 'devrel-mode-update-all-beams)
    (define-key map "\C-x\C-r\M-b" 'devrel-mode-update-rt-current-beam)
    (define-key map "\C-x\C-r\M-a" 'devrel-mode-update-all-rt-current-beams)
    (define-key map "\C-x\C-r\C-r" 'devrel-mode-display-running-nodes)
    (define-key map "\C-x\C-rms"   'devrel-mode-display-member-status)
    (define-key map "\C-x\C-rsn"   'devrel-mode-start-nodes)
    (define-key map "\C-x\C-r\C-c" 'devrel-mode-console)
    (define-key map "\C-x\C-rxn"   'devrel-mode-stop-nodes)
    (define-key map "\C-x\C-rrn"   'devrel-mode-restart-nodes)
    (define-key map "\C-x\C-r\C-x" 'devrel-mode-reset-nodes)
    (define-key map "\C-x\C-rcj"   'devrel-mode-cluster-join)
    (define-key map "\C-x\C-rcp"   'devrel-mode-cluster-plan)
    (define-key map "\C-x\C-rcc"   'devrel-mode-cluster-commit)
    (define-key map "\C-x\C-rcb"   'devrel-mode-cluster-build)
    map)
  "Keymap for devrel-mode")

(defconst devrel-mode-msgs-buffer-name "*msgs devrel-mode*")
(defconst devrel-mode-buffer-name "*devrel-mode*")

(define-minor-mode devrel-mode
  "Toggle devrel-mode. a mode for working w/ Riak devrels"
  ;; Init value
  nil
  ;; Indicator for mode line
  " devrel"
  devrel-mode-map
  :group 'devrel)

(add-hook 'erlang-mode-hook 'devrel-mode-start-hook)

(defun devrel-mode-start-hook ()
  "hook to start devrel-mode if in a riak dependency"
  (if (devrel-mode-buffer-valid-dep) (devrel-mode) nil))

(defun devrel-mode-buffer-valid-dep ()
  "returns true if the file belongs to a riak dependency (hardcoded :()"
  (let ((dep (devrel-mode-buffer-dep-name)))
    (or (string-equal "riak_core" dep)
        (string-equal "riak_kv" dep))))

;; END Mode Globals and Definitions

;; Interactive Functions
(defun devrel-mode-display-running-nodes ()
  "TODO docstring"
  (interactive)
  (let ((buf (get-buffer-create devrel-mode-buffer-name))
        (running (mapconcat 'identity (devrel-mode-running-nodes) "\n")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Running Nodes: \n")
      (insert "--------------\n")
      (insert running))
    (display-buffer buf)))

(defun devrel-mode-display-member-status ()
  "display member status (according to dev1)"
  (interactive)
  (let ((buf1 (get-buffer-create devrel-mode-msgs-buffer-name))
        (buf2 (get-buffer-create devrel-mode-buffer-name)))
    (with-current-buffer buf1 (erase-buffer))
    (devrel-mode-riak-admin-member-status (devrel-mode-buffer-riak-dir) "dev1")
    (with-current-buffer buf2
      (erase-buffer)
      (insert-buffer buf1))
    (display-buffer buf2)))

(defun devrel-mode-update-all-beams ()
  "updates cluster's lib dirs w/ all beam files for riak dep associated w/ current buffer"
  (interactive)
  (let ((beams (file-expand-wildcards (devrel-mode-buffer-beams-wildcard))))
    (loop for beam in beams do
          (devrel-mode-update-beams beam (devrel-mode-buffer-lib-dirs))))
  (message "updated beams for %s" (devrel-mode-buffer-dep-name)))

;; TODO: compile or force save to compile first (assuming edts mode)?
(defun devrel-mode-update-beam ()
  "updates the clusters lib dirs w/ beam file for current buffer"
  (interactive)
 (devrel-mode-update-beams (devrel-mode-buffer-beam-file-path) (devrel-mode-buffer-lib-dirs))
  (message "updated beams for %s" (file-name-nondirectory (buffer-file-name))))

(defun devrel-mode-update-all-rt-current-beams ()
  "updates cluster in RT_DEST_DIR/current's libdirs w/ beam file for current buffer"
  (interactive)
  (let ((libdirs (devrel-mode-rt-current-lib-dirs)))
    (when libdirs
      (let ((beams (file-expand-wildcards (devrel-mode-buffer-beams-wildcard))))
        (loop for beam in beams do
              (devrel-mode-update-beams beam libdirs)))
      (call-process "git" nil (get-buffer-create devrel-mode-msgs-buffer-name) nil
                    "--git-dir"
                    (concat (getenv "RT_DEST_DIR") "/.git")
                    "--work-tree"
                    (getenv "RT_DEST_DIR")
                    "commit"
                    "-a"
                    "-m" "riak_test init"
                    "--amend")
      (message "updated rt/current beams for %s" (devrel-mode-buffer-dep-name)))))


;; TODO: compile or force save to compile first (assuming edts mode)?
(defun devrel-mode-update-rt-current-beam ()
  "updates the cluster in RT_DEST_DIR/current's libdirs w/ beam file for current buffer"
  (interactive)
  (let ((libdirs (devrel-mode-rt-current-lib-dirs)))
    (when libdirs
      (devrel-mode-update-beams (devrel-mode-buffer-beam-file-path) libdirs)
      (call-process "git" nil (get-buffer-create devrel-mode-msgs-buffer-name) nil
                    "--git-dir"
                    (concat (getenv "RT_DEST_DIR") "/.git")
                    "--work-tree"
                    (getenv "RT_DEST_DIR")
                    "commit"
                    "-a"
                    "-m" "riak_test init"
                    "--amend")
      (message "updated rt/current beams for %s" (file-name-nondirectory (buffer-file-name))))))

(defun devrel-mode-start-nodes (nodes)
  "TODO docstring"
  (interactive "sstart nodes (devN[,devN,...,devN]): ")
  (let ((nodes-list (split-string nodes ",")))
        (devrel-mode-riak-start-nodes (devrel-mode-buffer-riak-dir) nodes-list)))

(defun devrel-mode-console (node)
  "TODO docstring"
  (interactive "sconsole for (devN): ")
  (devrel-mode-riak-console (devrel-mode-buffer-riak-dir) node)
  (display-buffer (concat node "@127.0.0.1")))

(defun devrel-mode-stop-nodes (nodes)
  "TODO docstring"
  (interactive "sstop nodes (devN[,devN,...,devN): ")
  (let ((nodes-list (split-string nodes ",")))
    (devrel-mode-riak-stop-nodes (devrel-mode-buffer-riak-dir) nodes-list)))

(defun devrel-mode-restart-nodes (nodes)
  "TODO docstring"
  (interactive "srestart nodes (devN[,devN,...,devN]): ")
  (let ((nodes-list (split-string nodes ",")))
    (loop for node in nodes-list do
          (devrel-mode-riak-stop (devrel-mode-buffer-riak-dir) node)
          (devrel-mode-riak-start (devrel-mode-buffer-riak-dir) node))))

(defun devrel-mode-reset-nodes (sure)
  "TODO docstring"
  (interactive "care you sure? [y/N]: ")
  (if (= 121 (downcase sure))
      (progn
        (devrel-mode-riak-stop-nodes (devrel-mode-buffer-riak-dir) (devrel-mode-running-nodes))
        (loop for path in (devrel-mode-buffer-data-dirs) do
              (call-process "rm"
                            nil (get-buffer-create devrel-mode-msgs-buffer-name) nil "-r" path))
        (message "nodes reset"))
    'ok))

(defun devrel-mode-cluster-join (node to)
  "TODO docstring"
  (interactive "sjoin node (devN): \nsto (devN): ")
  (devrel-mode-riak-admin-cluster-join (devrel-mode-buffer-riak-dir) node (concat to "@127.0.0.1"))
  (message "staged join of %s to %s" node to))

;; TODO: output plan to buffer
(defun devrel-mode-cluster-plan ()
  "TODO docstring. uses dev1"
  (interactive)
  (let ((buf1 (get-buffer-create devrel-mode-msgs-buffer-name))
        (buf2 (get-buffer-create devrel-mode-buffer-name)))
    (with-current-buffer buf1 (erase-buffer))
    (devrel-mode-riak-admin-cluster-plan (devrel-mode-buffer-riak-dir) "dev1")
    (with-current-buffer buf2
      (erase-buffer)
      (insert-buffer buf1))
    (display-buffer buf2)))

(defun devrel-mode-cluster-commit ()
  "TODO docstring. uses dev1"
  (interactive)
  (let ((buf1 (get-buffer-create devrel-mode-msgs-buffer-name))
        (buf2 (get-buffer-create devrel-mode-buffer-name)))
    (with-current-buffer buf1 (erase-buffer))
    (devrel-mode-riak-admin-cluster-commit (devrel-mode-buffer-riak-dir) "dev1")
    (with-current-buffer buf2
      (erase-buffer)
      (insert-buffer buf1))
    (display-buffer buf2)))

(defun devrel-mode-cluster-build (nodes)
  "builds fresh cluster, resetting one thats running"
  (interactive "sbuild cluster with nodes: (devN,devN,[,devN])")
  (let ((nodes-list (split-string nodes ",")))
    (devrel-mode-reset-nodes 121)
    (devrel-mode-start-nodes nodes)
    (loop for n in (cdr nodes-list) do
          (devrel-mode-cluster-join n (car nodes-list)))
    (devrel-mode-cluster-plan)
    (devrel-mode-cluster-commit)
    (devrel-mode-display-member-status)))

;; END interactive functions

(defun devrel-mode-running-nodes ()
  "TODO docstring"
  (loop for path in (devrel-mode-buffer-dev-dirs)
        if (= 0 (devrel-mode-riak-ping (devrel-mode-buffer-riak-dir) (file-name-nondirectory path)))
        collect (file-name-nondirectory path)))

(defun devrel-mode-update-beams (beam dirs)
  (if (null dirs) 'ok
    (progn
      (call-process "cp" nil (get-buffer-create devrel-mode-msgs-buffer-name) nil beam (car dirs))
      (devrel-mode-update-beams beam (cdr dirs)))))

(defun devrel-mode-rt-current-lib-dirs ()
  "returns list of lib directories in RT_DEST_DIR/riak/current. nil returned if env var not set"
  (let ((rtpath (getenv "RT_DEST_DIR")))
    (when rtpath
      (file-expand-wildcards (devrel-mode-rt-current-lib-wildcard rtpath)))))

(defun devrel-mode-rt-current-lib-wildcard (base)
  "returns wildcard sdtring to search for lib directories inside rtpath"
  (concat base
          "/current/dev/dev*/lib/"
          (devrel-mode-buffer-dep-name)
          "*/ebin"))

(defun devrel-mode-buffer-riak-dir ()
  "returns riak dir for this buffer. assumes ../riak"
  (concat (devrel-mode-buffer-root-path) "../riak"))

(defun devrel-mode-buffer-data-dirs ()
  "returns lib of data directories for this riak dep. assumes ../riak"
  (file-expand-wildcards (devrel-mode-buffer-data-wildcard)))

(defun devrel-mode-buffer-lib-dirs ()
  "returns list of lib directories for this riak dep. assumes ../riak"
  (file-expand-wildcards (devrel-mode-buffer-lib-wildcard)))

(defun devrel-mode-buffer-dev-dirs ()
  "returns list of dev* directories for this riak dep. assumes ../riak"
  (file-expand-wildcards (devrel-mode-buffer-dev-wildcard)))

(defun devrel-mode-buffer-data-wildcard ()
  "returns wildcard string to search for data directories for this riak dep. assumes ../riak"
  (concat (devrel-mode-buffer-dev-wildcard) "/data"))

;; TODO use dev-wildcard and riak-dir
(defun devrel-mode-buffer-lib-wildcard ()
  "return wildcard string to search for lib directories for this riak dependency. assumes ../riak"
  (concat (devrel-mode-buffer-root-path)
          "../riak/dev/dev*/lib/"
          (devrel-mode-buffer-dep-name)
          "*/ebin"))

;; TODO use riak-dir
(defun devrel-mode-buffer-dev-wildcard ()
  "return wildcard string to search for dev* directories for this riak dep. assumes ../riak"
  (concat (devrel-mode-buffer-root-path)
          "../riak/dev/dev*"))

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

(defun devrel-mode-buffer-beams-wildcard ()
  "return wildcard string that matches all beam files corresponding to dependency buffer belongs to"
  (concat (file-name-directory (buffer-file-name)) "../ebin/*.beam"))

(defun devrel-mode-riak-console (riak-path node)
  "bin/riak console"
  (let* ((node-name (concat node "@127.0.0.1"))
        (path (concat riak-path "/dev/" node))) ;; TODO: deal w/ "/" (filename:join equiv?)
    ;; TODO: turn off edts mode?
    (message "starting console for %s" node-name)
    (edts-shell-make-comint-buffer
     node-name node-name path
     (list "bin/riak" "console"))))

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

(defun devrel-mode-riak-stop-nodes (riak-path nodes)
  "stop nodes"
  (loop for node in nodes do
        (devrel-mode-riak-stop riak-path node)))

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
  "runs command synchronously, dumping to output to the devrel-mode-messages-buffer-name
buf. exit status returned"
  (let* ((bin (concat "bin/" bin-file))
         (node-name (concat node "@127.0.0.1"))
         (dir-before (substring (pwd) 10))
         (path (concat riak-path "/dev/" node))
         (full-bin (concat path "/" bin))) ;; TODO: deal w/ "/" (filename:join equiv?)
    (apply #'call-process full-bin nil (get-buffer-create devrel-mode-msgs-buffer-name) nil cmd)))

(defun devrel-mode-exit-message (result success-msg success-args failure-msg failure-args)
  "notifies result of process based on exit status"
  (progn
    (if (= result 0)
        (apply #'message success-msg success-args)
      (apply #'message failure-msg failure-args))
    result))

(provide 'devrel-mode)
