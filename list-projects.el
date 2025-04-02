;; TODO
;; - Link to project root DONE
;; - Include name as well as full path DONE
;; - Currently open buffers? DONE (links to list of buffers in project)
;; - Change default directory in a pre-command hook (so commands will
;;    work relative to the project at point) DONE
;; - Show VC (git/hg/whatever) DONE
;; - Revert-buffer-hook or something (g should refresh the view) DONE
;; - Have point stay on whatever we're currently on after a refresh DONE
;; - Fix project menu showing up in buffer list for given project DONE
;; - Correct project buffer count  DONE
;; - Also fix project menu counting towards buffer count for the
;;    project point is currently on when we revert DONE
;; - Have other buffer open in different window and have point
;;    automatically switch to that window for all things DONE
;; - Show current compilation status BLOCKED (I don't think that
;;    information is even stored?)
;; - Fix project menu showing up in buffer list when hitting c-x p c-b
;;    (list buffers) (might not be possible?)
;; - Also fix project menu showing up in buffer list when hitting g in
;;    buffer list DONE
;; - Improve performance of project buffer counts (n = number of buffers,
;;    p = number of projects. Currently O(np), should be O(n))
;;    Hmmm but actually it's not that simple, because you could maybe
;;    have nested projects. I think possibly the way you would have to
;;    do this is with string tries?
;;    It's also just not that simple cus you're looking for some item
;;    which is a prefix of this item. I think you need string tries
;;    for this no matter what??
;;    Ignore nested projects: things are associated with the project
;;    with the longest common prefix
;; - Support arbitrary list of projects DONE
;; - Modify project.el to exclude the *Projects* buffer when listing
;;    buffers in a project

;; List projects keybinding C-x p l

;;; Code:
(require 'project)

(defface project-name
  '((t :inherit link))
  "Face used on project names in the project menu."
  :version "29.0")

(defface project-buffers-count
  '((t :inherit link))
  "Face used on the buffer count in the project menu."
  :version "29.0")

(defface project-vc
  '((t :inherit link))
  "Face used on version control system names in the project menu."
  :version "29.0")

(defcustom project-name-column-width 30
  "Column width for the Project name in the project menu."
  :type 'natnum
  :version "29.0")

(defcustom project-root-column-width 40
  "Column width for the Project root in the project menu."
  :type 'natnum
  :version "29.0")

(defcustom project-buffers-count-column-width 10
  "Column width for the Project buffers count in the project menu."
  :type 'natnum
  :version "29.0")

(defcustom project-vc-column-width 10
  "Column width for the Project VC in the project menu."
  :type 'natnum
  :version "29.0")

(defcustom project-listing-function #'project-known-projects
  "Default function to use to get a list of projects."
  :type 'function
  :version "29.0")

(defvar-local project-menu-refresh-fun nil
  "The function to return a list of projects.")

(defun project-menu--buffer-predicate (A B)
  "Predicate to sort \"*Projects*\" buffer by number of project buffers.
This is used for 'tabulated-list-format' in 'project-menu-mode'."
  (let ((dA (string-to-number (car (aref (cadr A) 2))))
        (dB (string-to-number (car (aref (cadr B) 2)))))
    (if (= dA dB)
        (let ((name-A (car (aref (cadr A) 0)))
              (name-B (car (aref (cadr B) 0))))
          (string< name-A name-B))
      (< dA dB))))

(defun project--ensure-project-menu-mode ()
  "Signal a user-error if major mode is not `project-menu-mode'."
  (unless (derived-mode-p 'project-menu-mode)
    (user-error "The current buffer is not a Project Menu")))

(defun project-menu--refresh-contents (&optional _arg _noconfirm)
  (project--ensure-project-menu-mode)
  (project-menu--refresh))

(defun project-menu-jump-to-project (button)
  "Triggered when clicking/reting on a project name.
Opens the project's root directory in 'Dired'."
  (let ((proj (button-get button 'project)))
    (pop-to-buffer (dired-noselect (project-root proj)))))

(defun project-menu--list-buffers (button)
  "Lists buffers for the project on the current row."
  (interactive nil project-menu-mode)
  (let* ((proj (button-get button 'project))
         (project-current-directory-override (project-root proj))
         (default-directory "/")) ;; Set default directory to avoid the Packages buffer showing up in the list of buffers
    (select-window (project-list-buffers))))

(defun project-menu--show-vc-state (button)
  "Triggered when clicking/reting on the VC system for a project.
Opens the summary view for the corresponding VC system in the
project's root directory"
  (interactive nil project-menu-mode)
  (let ((proj (button-get button 'project))
        (vc (button-get button 'vc)))
    ;; Use magit if magit is installed and this is a git repo
    (if (and (eq vc 'Git)
             (package-installed-p 'magit))
        (magit-status (project-root proj))
      (vc-dir (project-root proj)))))

;; Trie implementation for fast project-buffer lookups
(defun project-build-trie (projects)
  "Build a trie from PROJECTS for fast prefix matching using hash tables."
  (let ((trie (make-hash-table :test 'equal)))
    (dolist (proj projects)
      (let* ((project-root (expand-file-name (project-root proj)))
             ;; Ensure the path ends with a slash
             (normalized-root (file-name-as-directory project-root))
             (path-components (split-string normalized-root "/" t))
             (current-node trie))
        ;; Insert the project path into the trie
        (dolist (component path-components)
          (let ((next-node (gethash component current-node)))
            (unless next-node
              (setq next-node (make-hash-table :test 'equal))
              (puthash component next-node current-node))
            (setq current-node next-node)))
        ;; Mark the end of a project path with the project object
        (puthash :project proj current-node)))
    trie))

(defun project-for-buffer (buffer trie)
  "Find project for BUFFER using TRIE-based prefix matching."
  (let* ((buffer-dir (with-current-buffer buffer 
                       (expand-file-name default-directory)))
         ;; Ensure the path ends with a slash
         (normalized-dir (file-name-as-directory buffer-dir))
         (path-components (split-string normalized-dir "/" t))
         (current-node trie)
         (matched-project nil))
    ;; Traverse the trie to find the longest matching prefix
    (dolist (component path-components)
      (let ((next-node (gethash component current-node)))
        (when next-node
          (setq current-node next-node)
          (let ((project-marker (gethash :project current-node)))
            (when project-marker
              (setq matched-project project-marker))))))
    matched-project))

(defun group-buffers-by-project (projects)
  "Group all buffers by their PROJECTS using the trie-based lookup.
Returns a hash table mapping projects to lists of buffers."
  (let ((trie (project-build-trie projects))
        (project-buffers (make-hash-table :test 'equal)))
    ;; Assign buffers to projects
    (dolist (buffer (buffer-list))
      (let ((project (project-for-buffer buffer trie)))
        (when project
          (puthash project 
                   (cons buffer (gethash project project-buffers nil))
                   project-buffers))))
    project-buffers))

;; Modified function to use the precomputed buffer assignments
(defun project-menu--print-info-simple (proj buffer-map)
  "Gets info for PROJ using BUFFER-MAP of precomputed buffer assignments."
  (let* ((buffers (seq-filter
                   (lambda (buf)
                     (let ((name (buffer-name buf)))
                       (not (or
                             (equal name "*Projects*")
                             (string= (substring name 0 1) " ")))))
                   (gethash proj buffer-map nil)))
         (num-buffers (length buffers))
         (vc (project-try-vc (project-root proj))))
    (list proj
          `[
            ;; First column: project name
            ;; Links to dired of project root
            (,(project-name proj)
             face project-name
             font-lock-face project-name
             follow-link t
             project ,proj
             action project-menu-jump-to-project)
            ;; Second column: project root
            (,(project-root proj)
             face link
             font-lock-face link
             follow-link t
             project ,proj
             action project-menu-jump-to-project)
            ;; Third column: number of open buffers (blank if 0)
            ;; Links to list of open buffers if nonzero
            ,(if (> num-buffers 0) `(,(number-to-string num-buffers)
                                     face project-buffers-count
                                     font-lock-face project-buffers-count
                                     follow-link t
                                     project ,proj
                                     action project-menu--list-buffers)
               '(""))
            ;; Fourth column: vc
            ,(if vc
                 `(,(symbol-name (cadr vc))
                   face project-vc
                   font-lock-face project-vc
                   follow-link t
                   project ,proj
                   vc ,(cadr vc)
                   action project-menu--show-vc-state)
               `("N/A"
                 face project-vc
                 font-lock-face project-vc))])))

;; Modified refresh function to use the optimized buffer assignment
(defun project-menu--refresh ()
  "Populates the project menu with all known projects."
  (let* ((projects (funcall project-menu-refresh-fun))
         (buffer-map (group-buffers-by-project projects)))
    (setq tabulated-list-entries
          (mapcar (lambda (proj) 
                    (project-menu--print-info-simple proj buffer-map))
                  projects)))
  (tabulated-list-print t))


(defun project-known-projects ()
  "Return all known projects as project objects."
  (let ((proj-roots (delete-dups (mapcar #'expand-file-name (project-known-project-roots)))))
    ;; Get project object for each of these roots
    (mapcan (lambda (d) (let ((p (project--find-in-directory d)))
                          ;; Exclude non-existant projects
                          (if p (list p) nil)))
            proj-roots)))

(define-derived-mode project-menu-mode tabulated-list-mode "Project menu"
  :interactive nil
  ;; Set the default directory to whatever is at point
  (add-hook 'pre-command-hook 'project-menu--set-default-directory nil t)
  (setq tabulated-list-format
        `[("Project" ,project-name-column-width t)
          ("Root" ,project-root-column-width t)
          ("Buffers" ,project-buffers-count-column-width project-menu--buffer-predicate)
          ("VC" ,project-vc-column-width t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'project-menu--refresh-contents nil t))

(defvar-local project-menu--old-default-directory nil
  "The original `default-directory' before `project-menu--set-default-directory'.

If this is non-nil, it indicates that
`project-menu--set-default-directory' has set
`default-directory'; it will set `default-directory' back to this
if there's no project at point.")

(defun project-menu--set-default-directory ()
  "Sets the default directory to the root of the project for the row
currently at point, so that any commands are relative to this project"
  ;; Get the project our point is currently on
  (if-let ((proj (tabulated-list-get-id)))
      ;; Set default-directory to that root
      (progn
        (unless project-menu--old-default-directory
          (setq project-menu--old-default-directory default-directory))
        (setq default-directory (file-name-as-directory (project-root proj))))
    ;; If there's no project at point, reset back to the original default-directory
    (when project-menu--old-default-directory
      (setq default-directory project-menu--old-default-directory)
      (setq project-menu--old-default-directory nil))))

(defun list-projects ()
  "Create a project menu buffer."
  (interactive)
  (pop-to-buffer-same-window (list-projects-noselect)))

(defun list-projects-noselect (&optional project-list-fun)
  "Create a Project Menu buffer and return it.

If PROJECT-LIST-FUN is non-nil, it should be a function that returns
a list of projects; it means list those projects and no others."
  (let ((buf (get-buffer-create "*Projects*")))
    (with-current-buffer buf
      (project-menu-mode)
      (setq project-menu-refresh-fun
            (if project-list-fun
                project-list-fun
              #'project-known-projects))
      (project-menu--refresh-contents))
    buf))

(provide 'list-projects)
;;; list-projects.el ends here


(require 'benchmark)
(require 'cl-lib)

(defun generate-test-projects (n-projects)
  "Generate N-PROJECTS test project objects."
  (cl-loop for i from 1 to n-projects
           collect (let ((project-root (format "/home/user/projects/project-%03d" i)))
                     (cons 'transient project-root))))

(defun generate-test-buffers (n-buffers projects)
  "Generate N-BUFFERS test buffers distributed among PROJECTS."
  (let* ((n-projects (length projects))
         (buffers '()))
    (dotimes (i n-buffers)
      ;; Choose a random project
      (let* ((project-idx (random n-projects))
             (project (nth project-idx projects))
             (project-root (cdr project))
             ;; Create a buffer with default-directory in the project
             (subdir (format "%s/src/module-%d" project-root (random 5)))
             (buffer-name (format "buffer-%04d.el" i))
             (buffer (generate-new-buffer buffer-name)))
        ;; Set the buffer's default-directory
        (with-current-buffer buffer
          (setq default-directory (file-name-as-directory subdir)))
        (push buffer buffers)))
    buffers))

(defun original-assign-buffers-to-projects (projects buffers)
  "Assign BUFFERS to PROJECTS using the original O(N*P) algorithm."
  (let ((project-buffers (make-hash-table :test 'equal)))
    (dolist (buffer buffers)
      (let ((buffer-dir (with-current-buffer buffer default-directory))
            (matched-project nil))
        ;; For each buffer, check all projects to find a match
        (dolist (proj projects)
          (let ((project-root (file-name-as-directory 
                               (expand-file-name (cdr proj)))))
            (when (string-prefix-p project-root buffer-dir)
              (setq matched-project proj))))
        (when matched-project
          (puthash matched-project 
                   (cons buffer (gethash matched-project project-buffers nil))
                   project-buffers))))
    project-buffers))

(defun benchmark-project-buffer-assignment ()
  "Benchmark the original vs trie-based project-buffer assignment algorithms."
  (interactive)
  ;; Generate test data
  (let* ((n-projects 100)
         (n-buffers 1000)
         (projects (generate-test-projects n-projects))
         (buffers (generate-test-buffers n-buffers projects))
         (gc-cons-threshold most-positive-fixnum) ;; Prevent GC during benchmark
         original-result
         trie-result)
    
    (message "Starting benchmark with %d projects and %d buffers..." n-projects n-buffers)
    
    ;; Benchmark the original algorithm
    (message "Benchmarking original algorithm...")
    (let ((original-time
           (benchmark-run 10
             (setq original-result (original-assign-buffers-to-projects projects buffers)))))
      (message "Original algorithm: %.6f seconds average (%.6f seconds total for 10 runs)"
               (/ (car original-time) 10.0) (car original-time)))
    
    ;; Benchmark our trie-based algorithm
    (message "Benchmarking trie-based algorithm...")
    (let ((trie-time
           (benchmark-run 10
             (let ((trie (project-build-trie projects)))
               (setq trie-result (make-hash-table :test 'equal))
               (dolist (buffer buffers)
                 (let ((project (project-for-buffer buffer trie)))
                   (when project
                     (puthash project 
                              (cons buffer (gethash project trie-result nil))
                              trie-result))))))))
      (message "Trie-based algorithm: %.6f seconds average (%.6f seconds total for 10 runs)"
               (/ (car trie-time) 10.0) (car trie-time)))
    
    ;; Check that the results are equivalent
    (let ((original-count 0)
          (trie-count 0))
      (maphash (lambda (_k v) (cl-incf original-count (length v))) original-result)
      (maphash (lambda (_k v) (cl-incf trie-count (length v))) trie-result)
      (message "Original algorithm assigned %d buffers to projects" original-count)
      (message "Trie-based algorithm assigned %d buffers to projects" trie-count))
    
    ;; Clean up the test buffers
    (dolist (buffer buffers)
      (kill-buffer buffer))))

;; To run the benchmark:
;; (benchmark-project-buffer-assignment)
