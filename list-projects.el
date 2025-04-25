;;; list-projects.el -- Support for listing all the projects Emacs knows about -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Author: Matthew Tromp
;; Homepage: https://github.com/MatthewTromp/list-projects
;; Package-Requires: ((emacs "28.1"))

;; Copyright (C) 2024-2025 Matthew Tromp

;; This file is distributed under the terms of the GNU general public
;; license v.3

;;; Commentary:

;; List Projects provides an interactive tabulated list of all
;; projects known to Emacs.  It gives you a centralized view of your
;; projects with useful information and actions.
;;
;; Features
;; - Shows project names, root directories, open buffer counts, and version control status
;; - Fast buffer-to-project assignment using an efficient trie-based algorithm
;; - Clickable items to:
;;   - Open project roots in Dired
;;   - List all buffers belonging to a project
;;   - View version control status (with Magit integration for Git repositories)
;; - Sets the default directory to the project at point for command context
;;
;; Usage:
;; M-x list-projects         - Display the project list buffer
;; In the project list buffer:
;; - Click on a project name or root to open in Dired
;; - Click on the buffer count to list project buffers
;; - Click on the VC system to view version control status
;; - Regular Emacs commands are executed in the context of the project at point
;;
;; You can customize the appearance via:
;; - list-projects-project-name-column-width
;; - list-projects-project-root-column-width
;; - list-projects-project-buffers-count-column-width
;; - list-projects-project-vc-column-width
;;
;; To use a different function for listing projects, set:
;; - list-projects-default-listing-function

;; TODO
;; - Show current compilation status BLOCKED (I don't think that
;;    information is even stored?)
;; - Fix project list showing up in buffer list when hitting c-x p c-b
;;    (list buffers) (might not be possible? requires changes to project.el?)

;;; Code:

(require 'project)

(defface list-projects-project-name
  '((t :inherit link))
  "Face used on project names in the project list."
  :version "29.0")

(defface list-projects-project-buffers-count
  '((t :inherit link))
  "Face used on the buffer count in the project list."
  :version "29.0")

(defface list-projects-project-vc
  '((t :inherit link))
  "Face used on version control system names in the project list."
  :version "29.0")

(defgroup list-projects nil
  "List all the projects Emacs knows about."
  :group 'project)

(defcustom list-projects-project-name-column-width 30
  "Column width for the Project name in the project list."
  :type 'natnum
  :group 'project
  :version "29.0")

(defcustom list-projects-project-root-column-width 40
  "Column width for the Project root in the project list."
  :type 'natnum
  :group 'project
  :version "29.0")

(defcustom list-projects-project-buffers-count-column-width 10
  "Column width for the Project buffers count in the project list."
  :type 'natnum
  :group 'project
  :version "29.0")

(defcustom list-projects-project-vc-column-width 10
  "Column width for the Project VC in the project list."
  :type 'natnum
  :group 'project
  :version "29.0")

(defcustom list-projects-default-listing-function #'list-projects-known-projects
  "Default function to use to get a list of projects.
Must return project objects.  You can construct these from project roots
with `project--find-in-directory."
  :type 'function
  :group 'project
  :version "29.0")

(defvar-local list-projects-listing-function nil
  "A function returning the list of projects to be displayed in this buffer.")

(defun list-projects--buffer-predicate (A B)
  "Predicate to sort \"*Projects*\" buffer by number of project buffers.
This is used for `tabulated-list-format' in `list-projects-mode'.
A and B are the two rows to compare."
  (let ((dA (string-to-number (car (aref (cadr A) 2))))
        (dB (string-to-number (car (aref (cadr B) 2)))))
    (if (= dA dB)
        (let ((name-A (car (aref (cadr A) 0)))
              (name-B (car (aref (cadr B) 0))))
          (string< name-A name-B))
      (< dA dB))))

(defun list-projects-jump-to-project (button)
  "Triggered when clicking/reting on a project name.
Opens the project's root directory in `Dired'.
BUTTON is what the user pressed."
  (let ((proj (button-get button 'project)))
    (pop-to-buffer (dired-noselect (project-root proj)))))

(defun list-projects--list-buffers (button)
  "Lists buffers for the project on the current row.
BUTTON is what the user pressed."
  (interactive nil list-projects-mode)
  (let* ((proj (button-get button 'project))
         (project-current-directory-override (project-root proj))
         (default-directory "/")) ;; Set default directory to avoid the Packages buffer showing up in the list of buffers
    (select-window (project-list-buffers))))

(defun list-projects--show-vc-state (button)
  "Triggered when clicking/reting on the VC system for a project.
BUTTON is what the user pressed.

Opens the summary view for the corresponding VC system in the
project's root directory."
  (interactive nil list-projects-mode)
  (let ((proj (button-get button 'project))
        (vc (button-get button 'vc)))
    ;; Use magit if magit is installed and this is a git repo
    (if (and (eq vc 'Git)
             (package-installed-p 'magit))
        (progn (require 'magit)
               (magit-status (project-root proj)))
      (vc-dir (project-root proj)))))

(defun list-projects--build-trie (projects)
  "Build a trie from PROJECTS for fast prefix matching.

This is used when figuring out what buffers are in which projects.
Using a trie works in O((B+P)*L) where B is the number of buffers, P is
the number of projects and L is the maximum path length of a project's
root directory.

Using `project-buffers' would take O(B*P), because it iterates over
`buffer-list'."
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

(defun list-projects--project-for-buffer (buffer trie)
  "Find project for BUFFER using trie-based prefix matching.
TRIE is a tree of projects computed with `list-projects--build-trie'."
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

(defun list-projects--group-buffers-by-project (projects)
  "Group all buffers by their PROJECTS using the trie-based lookup.
Returns a hash table mapping projects to lists of buffers."
  (let ((trie (list-projects--build-trie projects))
        (project-buffers (make-hash-table :test 'equal)))
    ;; Assign buffers to projects
    (dolist (buffer (buffer-list))
      (let ((project (list-projects--project-for-buffer buffer trie)))
        (when project
          (puthash project
                   (cons buffer (gethash project project-buffers nil))
                   project-buffers))))
    project-buffers))

(defun list-projects--print-info-simple (proj buffer-map)
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
             face list-projects-project-name
             font-lock-face list-projects-project-name
             follow-link t
             project ,proj
             action list-projects-jump-to-project)
            ;; Second column: project root
            (,(project-root proj)
             face link
             font-lock-face link
             follow-link t
             project ,proj
             action list-projects-jump-to-project)
            ;; Third column: number of open buffers (blank if 0)
            ;; Links to list of open buffers if nonzero
            ,(if (> num-buffers 0) `(,(number-to-string num-buffers)
                                     face list-projects-project-buffers-count
                                     font-lock-face list-projects-project-buffers-count
                                     follow-link t
                                     project ,proj
                                     action list-projects--list-buffers)
               '(""))
            ;; Fourth column: vc
            ,(if vc
                 `(,(symbol-name (cadr vc))
                   face list-projects-project-vc
                   font-lock-face list-projects-project-vc
                   follow-link t
                   project ,proj
                   vc ,(cadr vc)
                   action list-projects--show-vc-state)
               `("N/A"
                 face list-projects-project-vc
                 font-lock-face list-projects-project-vc))])))

(defun list-projects--entries ()
  "Entries function for `list-projects-mode'."
  (let* ((projects (funcall list-projects-listing-function))
         (buffer-map (list-projects--group-buffers-by-project projects)))
    (mapcar (lambda (proj)
              (list-projects--print-info-simple proj buffer-map))
            projects)))


(defun list-projects-known-projects ()
  "Return all known projects as project objects."
  (let ((proj-roots (delete-dups (mapcar #'expand-file-name (project-known-project-roots)))))
    ;; Get project object for each of these roots
    (mapcan (lambda (d) (let ((p (project--find-in-directory d)))
                          ;; Exclude non-existant projects
                          (if p (list p) nil)))
            proj-roots)))

(define-derived-mode list-projects-mode tabulated-list-mode "Project list"
  :interactive nil
  ;; Set the default directory to whatever project is on the current row
  (add-hook 'pre-command-hook 'list-projects--set-default-directory nil t)
  (setq tabulated-list-format
        `[("Project" ,list-projects-project-name-column-width t)
          ("Root" ,list-projects-project-root-column-width t)
          ("Buffers" ,list-projects-project-buffers-count-column-width list-projects--buffer-predicate)
          ("VC" ,list-projects-project-vc-column-width t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'list-projects--entries)
  (tabulated-list-init-header))

(defvar-local list-projects--old-default-directory nil
  "The original `default-directory' before `list-projects--set-default-directory'.

If this is non-nil, it indicates that
`list-projects--set-default-directory' has set
`default-directory'; it will set `default-directory' back to this
if there's no project at point.")

(defun list-projects--set-default-directory ()
  "Set the default directory to the root of the selected project.
This is the project corresponding to the row that POINT is currently on.
This is so that any commands (e.g. `find-file') are relative to this
project."
  ;; Get the project our point is currently on
  (if-let ((proj (tabulated-list-get-id)))
      ;; Set default-directory to that root
      (progn
        (unless list-projects--old-default-directory
          (setq list-projects--old-default-directory default-directory))
        (setq default-directory (file-name-as-directory (project-root proj))))
    ;; If there's no project at point, reset back to the original default-directory
    (when list-projects--old-default-directory
      (setq default-directory list-projects--old-default-directory)
      (setq list-projects--old-default-directory nil))))

(defun list-projects ()
  "Create and display a project list buffer."
  (interactive)
  (pop-to-buffer-same-window (list-projects-noselect)))

(defun list-projects-noselect (&optional project-list-fun)
  "Create a project list buffer and return it.

If PROJECT-LIST-FUN is non-nil, it should be a function that returns
a list of projects; it means list those projects and no others."
  (let ((buf (get-buffer-create "*Projects*")))
    (with-current-buffer buf
      (list-projects-mode)
      (setq list-projects-listing-function
            (if project-list-fun
                project-list-fun
              list-projects-default-listing-function))
      (revert-buffer))
    buf))

(provide 'list-projects)
;;; list-projects.el ends here
