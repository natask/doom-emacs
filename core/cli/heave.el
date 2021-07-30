;;; core/cli/heave.el -*- lexical-binding: t; -*-

(defcli! (heave)
     (&rest packages)
"Update packages passed as argument."
  (print! (info "passed in %S") packages)
  (print-group!
   (doom-cli-package-list-update packages)
  t)
)

(defun doom-cli-package-list-update (packages)
  "Updates packages passed in as PACKAGES."
  (doom-initialize-packages)
  (doom--barf-if-incomplete-packages)
  (let* ((repo-dir (straight--repos-dir))
         ;(pinned (doom-package-pinned-list))
         (recipes (doom-package-recipe-list))
         (packages-to-rebuild (make-hash-table :test 'equal))
         (repos-to-rebuild (make-hash-table :test 'equal))
         (total (length packages))
         (esc (unless doom-debug-p "\033[1A"))
         (i 0)
         installed-packages
         errors)
    (when recipes
      (doom--cli-recipes-update))
    (print! (start "Updating packages (this may take a while)..."))
    (doom--with-package-recipes recipes (recipe package type local-repo)
    (when (member package packages) ;;added line
      (print! (info "Found package: %s") package)
      (push package installed-packages)
      (cl-incf i)
      (print-group!
       (unless (straight--repository-is-available-p recipe)
         (print! (error "(%d/%d) Couldn't find local repo for %s") i total package)
         (cl-return))
       (when (gethash local-repo repos-to-rebuild)
         (puthash package t packages-to-rebuild)
         (print! (success "(%d/%d) %s was updated indirectly (with %s)") i total package local-repo)
         (cl-return))
       (let ((default-directory (straight--repos-dir local-repo)))
         (unless (file-in-directory-p default-directory repo-dir)
           (print! (warn "(%d/%d) Skipping %s because it is local") i total package)
           (cl-return))
         (when (eq type 'git)
           (unless (file-exists-p ".git")
             (error "%S is not a valid repository" package)))
         (condition-case-unless-debug e
             (let ((ref (straight-vc-get-commit type local-repo))
                   target-ref
                   commits
                   output)
               (or (cond
                    ((not (stringp target-ref))
                     (print! (start "\033[K(%d/%d) Fetching %s...%s") i total package esc)
                     (doom--straight-with (straight-vc-fetch-from-remote recipe)
                       (when .it
                         (straight-merge-package package)
                         (setq target-ref (straight-vc-get-commit type local-repo))
                         (setq output (doom--commit-log-between ref target-ref)
                               commits (length (split-string output "\n" t)))
                         (or (not (doom--same-commit-p target-ref ref))
                             (cl-return)))))

                    ((doom--same-commit-p target-ref ref)
                     (print! (info "\033[K(%d/%d) %s is up-to-date...%s") i total package esc)
                     (cl-return))

                    ((if (straight-vc-commit-present-p recipe target-ref)
                         (print! (start "\033[K(%d/%d) Checking out %s (%s)...%s")
                                 i total package (doom--abbrev-commit target-ref) esc)
                       (print! (start "\033[K(%d/%d) Fetching %s...%s") i total package esc)
                       (and (straight-vc-fetch-from-remote recipe)
                            (straight-vc-commit-present-p recipe target-ref)))
                     (straight-vc-check-out-commit recipe target-ref)
                     (or (not (eq type 'git))
                         (setq output (doom--commit-log-between ref target-ref)
                               commits (length (split-string output "\n" t))))
                     (doom--same-commit-p target-ref (straight-vc-get-commit type local-repo)))

                    ((print! (start "\033[K(%d/%d) Re-cloning %s...") i total local-repo esc)
                     (let ((repo (straight--repos-dir local-repo))
                           (straight-vc-git-default-clone-depth 'full))
                       (delete-directory repo 'recursive)
                       (print-group!
                        (straight-use-package (intern package) nil 'no-build))
                       (prog1 (file-directory-p repo)
                         (or (not (eq type 'git))
                             (setq output (doom--commit-log-between ref target-ref)
                                   commits (length (split-string output "\n" t))))))))
                   (progn
                     (print! (warn "\033[K(%d/%d) Failed to fetch %s")
                             i total local-repo)
                     (unless (string-empty-p output)
                       (print-group! (print! (info "%s" output))))
                     (cl-return)))
               (puthash local-repo t repos-to-rebuild)
               (puthash package t packages-to-rebuild)
               (print! (success "\033[K(%d/%d) %s: %s -> %s%s")
                       i total local-repo
                       (doom--abbrev-commit ref)
                       (doom--abbrev-commit target-ref)
                       (if (and (integerp commits) (> commits 0))
                           (format " [%d commit(s)]" commits)
                         ""))
               (unless (string-empty-p output)
                 (let ((lines (split-string output "\n")))
                   (setq output
                         (if (> (length lines) 20)
                             (concat (string-join (cl-subseq (butlast lines 1) 0 20) "\n")
                                     "\n[...]")
                           output)))
                 (print-group! (print! "%s" (indent 2 output)))))
           (user-error
            (signal 'user-error (error-message-string e)))
           (error
            (signal 'doom-package-error (list package e))))))))
    (print-group!
     (princ "\033[K")
     (if (hash-table-empty-p packages-to-rebuild)
         (progn
         (ignore (print! (success "Installed %S") installed-packages))
         (ignore (print! (success "All %d packages are up-to-date") total)))
       (straight--transaction-finalize)
       (let ((default-directory (straight--build-dir)))
         (mapc (doom-rpartial #'delete-directory 'recursive)
               (hash-table-keys packages-to-rebuild)))
       (print! (success "Updated %d package(s)")
               (hash-table-count packages-to-rebuild))
       (doom-cli-packages-build)
       t))))
