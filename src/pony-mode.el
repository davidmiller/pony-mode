;;; pony-mode.el --- minor mode for working with Django projects

;; Copyright (C) 2015 David Miller <david@deadpansincerity.com>

;; Author: David Miller <david@deadpansincerity.com>
;; Maintainer: David Miller <david@deadpansincerity.com>
;; Created: 2011-02-20
;; Keywords: python django
;; URL: https://github.com/davidmiller/pony-mode
;; Version: 0.3b

;;

;; This file is NOT part of GNU Emacs

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:

;; Variables
(defgroup pony nil
  "Djangification for Emacs"
  :group 'programming
  :prefix "pony-")

(defcustom pony-etags-command "find . | grep .py | xargs etags"
  "Command to generate tags table for project"
  :group 'pony
  :type 'string)

(defcustom pony-server-host "localhost"
  "Host to run pony dev server"
  :group 'pony
  :type 'string)

(defcustom pony-server-port "8000"
  "Port to run pony dev server"
  :group 'pony
  :type 'string)

(defcustom pony-settings-module "settings"
  "Settings module to use with manage.py"
  :group 'pony
  :type 'string)

(defcustom pony-test-failfast t
  "Run pony tests with failfast?"
  :group 'pony
  :type 'bool)

(defcustom pony-sqlite-program "sqlite3"
  "Name of the executable to use when running a database REPL for Django
projects using sqlite."
  :group 'pony
  :type 'string)

(defcustom pony-snippet-dir (expand-file-name
                             (concat (file-name-directory (or load-file-name default-directory))
                                     "./snippets"))
  "Directory in which to locate Yasnippet snippets for Pony Mode"
  :group 'pony
  :type 'string)

(defcustom pony-enable-template-mode t
  "Enable Django template mode?"
  :group 'pony
  :type 'bool)

(defvar pony-filesystem-ceiling (if (eq 'windows-nt system-type)
                                    "c:/" "/"))

;; Dependancies and environment sniffing
(require 'cl)
(require 'dired-aux)
(ignore-errors ; files-x gets stripped from some Debian packages
  (require 'files-x))
(require 'python)
(require 'sgml-mode)
(require 'sql)
(require 'thingatpt)
(require 'which-func)

;; Utility

(defun pony-chomp (str)
  "Chomp leading and tailing whitespace www.emacswiki.org/emacs/ElispCookbook"
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun pony-find-file (path pattern)
  "Find files matching pattern in or below path"
  (setq matches (list))
  (let ((files (list)))
    (dolist (f-or-d
             (directory-files path t "^[^\\.]"))
      (if (file-directory-p f-or-d)
          (dolist (filename
                   (string-match pattern f-or-d))
            (add-to-list 'files filename))
        (if (string-match pattern f-or-d)
            (add-to-list 'files f-or-d))))
    files))

(defun pony-find-file-p (path pattern)
  "Predicate to determine whether a file whose name matches PATTERN is to be
found in or under PATH"
  (if (pony-find-file path pattern)
      t
    nil))

(defun pony-locate (filepath)
  "Essentially duplicates the functionality of `locate-dominating-file'
but allows paths rather than filenames"
  (let ((dir (expand-file-name default-directory))
        (found nil))
    (while (and (not (equal pony-filesystem-ceiling dir))
                (not found))
      (let ((check (concat dir filepath)))
        (if (file-exists-p check)
            (setq found check)))
      (setq dir (file-name-directory
                 (directory-file-name dir))))
    found))

;;;###autoload
(defun pony-read-file (filepath)
  "Read the contents of `filepath'"
  (with-temp-buffer
    (insert-file-contents filepath)
    (read (current-buffer))))

;;
;; Emacs
;;

(defun* pony-pop (buffer-or-name &key dirlocals)
  "Select `buffer-or-name' in some window, as for `pop-to-buffer'.
Return the selected buffer if successful, or `nil' otherwise.

If the optional keyword argument `:dirlocals' is non-nil, set the
variable `dir-local-variables-alist' in the target buffer to be
equal to the value in the current buffer.  This is useful because
comint buffers without filenames associated will otherwise not
pick up directory-local settings."
  (let ((buffer (get-buffer buffer-or-name))
        (current-locals dir-local-variables-alist))
    (when buffer
      (pop-to-buffer buffer)
      (pony-mode)
      (and dirlocals
           current-locals
           (pony-local! 'dir-local-variables-alist current-locals)))
    buffer))

(defun pony-comint-pop(name command args)
  "This is the main entry point for sub-processes in Pony-mode.
It creates a comint interaction buffer, called `name', running
`command', called with `args'"
  (ansi-color-for-comint-mode-on)
  (apply 'make-comint name command nil args)
  (pony-pop (concat "*" name "*") :dirlocals t))

(defun pony-manage-pop (name command args)
  "Run manage.py commands in a commint buffer. Intended as a
wrapper around `pony-commint-pop', this function bypasses the
need to construct manage.py calling sequences in command
functions."
  ;; !!! This API is utterly stupid. call (pony-manage-cmd)
  ;; here rather than in the caller.
  (let* ((settings (if (pony-project-newstructure-p)
                       (concat (pony-project-package) "."
                               (pony-get-settings-file-basename))
                     (pony-get-settings-file-basename)))
         (pythonpath (if (pony-project-newstructure-p)
                         (pony-get-pythonpath)
                       nil))
         (python-args
          (cons command (append args (list (concat "--settings=" settings)
                                           (concat "--pythonpath=" pythonpath))))))
    (pony-comint-pop name (pony-active-python) python-args)))

(defun pony-manage-popif (name command args)
  "This wrapper around `pony-manage-pop' will check to see if COMMAND exists
before attempting the manage-pop"
  (if (pony-command-exists-p command)
      (pony-manage-pop name (pony-manage-cmd) (cons command args))
    (message "The Django command %s doesn't seem to be installed" command)))

(defun pony-dir-excursion(dir &rest rest)
  "pony-comint-pop where we need to change into `dir` first"
  (let ((default-directory dir))
    (apply 'pony-comint-pop rest)))

(defun pony-mini-file(prompt &optional startdir)
  "Read a file from the minibuffer."
  (expand-file-name
   (read-file-name prompt
                   (or startdir
                       (expand-file-name default-directory)))))

(defun pony-local! (var val)
  "Set the buffer-local variable VAR to VAL.
Destructive function with no state checking - see `pony-localise' for a
more conservative local-var manipulation."
  (set (make-local-variable var) val))

(defun pony-localise (var func)
  "Return buffer local varible or get & set it"
  (if (local-variable-p var)
      (symbol-value var)
    (let ((the-var (funcall func)))
      (if the-var
          (progn
            (make-local-variable var)
            (set var the-var))))))

;; Pony-mode

;;
;; Config files
;;
;; Commentary:
;;
;; Allow us to specify things per-project where our local
;; setup is not one of the ones anticipated...
;;
;; We then read in the .ponyrc file in the `pony-project-root',
;; which should define a pony-project variable
;;

(defstruct pony-project python settings pythonpath appsdir)

(defun pony-configfile-p ()
  "Establish whether this project has a .ponyrc file in the root"
  (if (equal 'dired-mode major-mode)
      (if (pony-locate ".dir-locals.el") t nil)
    (if (or
         (and (buffer-file-name) (dir-locals-find-file (buffer-file-name)))
         (pony-rooted-sym-p '.ponyrc))
        t nil)))

(defun pony-rc ()
  "Return the settings for the current project.

Evaluate the pony-settings variable from the directory-local
variables; if not found, evaluate .ponyrc instead."
  (dolist (pair (rest (first (pony-read-file (pony-locate ".dir-locals.el")))) res)
    (if (equal 'pony-settings (first pair))
        (setq res (eval (first (rest pair))))
      nil)))

(when (featurep 'files-x)
;;;###autoload
  (defun pony-define-project ()
    "Create or alter the pony-project settings for the current project"
    (interactive)
    (let* ((localsfile (concat (pony-project-root) ".dir-locals.el"))
           (current (if (pony-configfile-p)
                        (pony-rc)
                      (make-pony-project)))
           (interpreter (read-from-minibuffer "Python: " (pony-project-python current)))
           (settings (read-from-minibuffer "Settings module: "
                                           (or (pony-project-settings current)
                                               pony-settings-module))))
      (if (not (file-exists-p localsfile))
          (dired-do-touch localsfile))
      (modify-dir-local-variable nil 'pony-settings '(write list here) 'delete)))
  )

;;;###autoload
(defun pony-reload-mode()
  (interactive)
  (load-library "pony-mode"))

;;
;; Python
;;
;; Commentary:
;;
;; Functions for getting contextually aware information
;; about the code near point
;;

(defun pony-get-app ()
  "Return the name of the current app, or nil if no app found."
  (let* ((root (concat (pony-project-root) (pony-get-appsdir)))
         (re (concat "^" (regexp-quote root) "\\([A-Za-z_]+\\)/"))
         (path (or buffer-file-name (expand-file-name default-directory))))
    (when (string-match re path)
      (match-string 1 path))))

(defun pony-get-module ()
  (let* ((root (pony-project-root))
         (path (file-name-sans-extension (or buffer-file-name (expand-file-name default-directory)))))
    (when (string-match (pony-project-root) path)
      (let ((path-to-class (substring path (match-end 0))))
        (mapconcat 'identity (split-string path-to-class "/") ".")))))

;; Environment

(defun pony-project-root()
  "Return the root of the project(dir with manage.py in) or nil"
  (pony-localise
   'pony-this-project-root
   '(lambda ()
      (let ((curdir default-directory)
            (max 10)
            (found nil))
        (while (and (not found) (> max 0))
          (progn
            (if (or (file-exists-p (concat curdir "/bin/django")) ; Buildout?
                    (file-exists-p (concat curdir "manage.py")))
                (progn
                  (setq found t))
              (progn
                (setq curdir (concat curdir "../"))
                (setq max (- max 1))))))
        (if found (expand-file-name curdir))))))

(defun pony-project-newstructure-p()
  "Predicate to determine whether the project has new structure.

In django ver. => 1.4 manage.py is in an upper directory relative to the
project module."
  (let ((settings-base
         (concat (pony-project-root)
                 (pony-get-settings-file-basename))))
    (not (or (file-exists-p (concat settings-base ".py"))
              (file-exists-p settings-base)))))

(defun pony-project-package()
  "Return the project package name."
  (pony-localise
   'pony-this-project-package
   '(lambda ()
      (let ((diffsettings nil)
            (package ""))
        (if (not (pony-project-newstructure-p))
            (setq package (file-name-nondirectory
                           (directory-file-name (pony-project-root))))
          (progn
            (setq diffsettings
                  (shell-command-to-string
                   (concat (pony-active-python) " "
                           (pony-manage-cmd) " "
                           "diffsettings")))
            (if (string-match "SETTINGS_MODULE = '\\([^'.]+\\)" diffsettings)
                (setq package (match-string 1 diffsettings)))))))))

(defun pony-project-package-root()
  "Return the root of the project packege (dir with project
settings.py in) or nil"
  (pony-localise
   'pony-this-project-package-root
   '(lambda ()
      (let ((package-root nil))
        (if (not (pony-project-newstructure-p))
            (setq package-root (pony-project-root))
          (progn
            (setq package-root (pony-project-package))
            (if package-root
                (expand-file-name (file-name-as-directory package-root)
                                  (pony-project-root)))))))))
(defun pony-rooted-sym-p (symb)
  "Expand the concatenation of `symb` onto `pony-project-root` and determine whether
that file exists"
  (file-exists-p (concat (pony-project-root) (symbol-name symb))))

(defun pony-manage-cmd()
  "Return the current manage command
This command will only work if you run with point in a buffer that is within your project"
  (let ((found nil)
        (virtualenv '../bin/activate)
        (cmds (list 'bin/django '../bin/django 'manage.py)))
    (if (pony-rooted-sym-p virtualenv)
        (expand-file-name (concat (pony-project-root) "manage.py"))
      ;; Otherwise, look for buildout, defaulting to the standard manage.py script
      (progn
        (dolist (test cmds)
          (if (and (not found) (pony-rooted-sym-p test))
              (setq found (expand-file-name
                           (concat (pony-project-root) (symbol-name test))))))
        (if found
            found)))))

(defun pony-active-python ()
  "Fetch the active Python interpreter for this Django project.
Be aware of .ponyrc configfiles, 'clean', buildout, and
 virtualenv situations"
  (if (and (pony-configfile-p)
           (pony-rc))
      (pony-project-python (pony-rc))
    (let ((venv-out (pony-locate "bin/python")))
      (if venv-out
          venv-out
        (executable-find "python")))))

(defun pony-command-exists-p(cmd)
  "Is cmd installed in this app"
  (if (string-match cmd
                    (shell-command-to-string (concat (pony-active-python)
                                                     " " (pony-manage-cmd))))
      t
    nil))

;;;###autoload
(defun pony-command-if-exists(proc-name command args)
  "Run `command` if it exists"
  (if (pony-command-exists-p command)
      (let ((process-buffer (concat "*" proc-name "*")))
        (progn
          (start-process proc-name process-buffer
                         (pony-active-python)
                         (pony-manage-cmd)
                         command args)
          (pop-to-buffer (get-buffer process-buffer))))
    nil))

;;;###autoload
(defun pony-get-settings-file-basename()
  "Return the name of the settings file to use for this
project. By default this is 'settings', but it can be changed
locally with .dir-locals.el."
  (if (pony-configfile-p)
      (let* ((rc (pony-rc))
             (settings (if rc
                           (pony-project-settings rc)
                         nil)))
        (if settings
            settings
          pony-settings-module))
    pony-settings-module))

(defun pony-get-pythonpath()
  "Return the customized pythonpath to be passed to manage.py.
This is configured in .dir-locals.el."
  (if (pony-configfile-p)
      (let* ((rc (pony-rc)))
        (if rc
            (pony-project-pythonpath rc)
          nil))
    nil))

(defun pony-get-appsdir()
  "Return the apps directory, relative to project root.
This is configured in .dir-locals.el."
  (if (pony-configfile-p)
      (let* ((rc (pony-rc)))
        (if rc
            (pony-project-appsdir rc)
          nil))
    nil))


(defun pony-get-settings-module()
  "Return the absolute path to the pony settings file"
  (let* ((basename (concat (pony-project-package-root)
                           (pony-get-settings-file-basename)))
         (pyfile (concat basename ".py"))
         (exists (or (and (file-exists-p basename) basename)
                     (and (file-exists-p pyfile) pyfile))))
    (if exists
        exists
      (progn
         (message "Settings file not found")
        nil))))

;;;###autoload
(defun pony-setting-p (setting)
  "Predicate to determine whether a `setting' exists for the current project"
  (let ((setting? (pony-get-setting setting)))
    (and setting?
         (if (string-match "Traceback" setting?)
             nil
           t))))

;;;###autoload
(defun pony-get-setting(setting)
  "Get the pony settings.py value for `setting`"
  (let ((settings (pony-get-settings-module))
        (python-c (concat (pony-active-python)
                          " -c \"import settings; print settings.%s\"")))
    (if settings
        (let ((default-directory (file-name-directory settings)))
          (pony-chomp (shell-command-to-string (format python-c setting)))))))

;;;###autoload
(defun pony-setting()
  "Interactively display a setting value in the minibuffer"
  (interactive)
  (let ((setting (read-from-minibuffer "Get setting: " (word-at-point))))
    (message (concat setting " : " (pony-get-setting setting)))))

;; Buildout

;;;###autoload
(defun pony-buildout-cmd()
  "Return the buildout command or nil if we're not in a buildout"
  (pony-localise
   'pony-this-buildout-root
   '(lambda ()
      (let ((root-parent
             (expand-file-name (concat (pony-project-root) "../"))))
        (if (file-exists-p
             (expand-file-name (concat root-parent "bin/buildout")))
            (expand-file-name (concat root-parent "bin/buildout"))
          nil)))))

;;;###autoload
(defun pony-buildout-list-bin()
  "List the commands available in the buildout bin dir"
  (directory-files (file-name-directory (pony-buildout-cmd))))

;;;###autoload
(defun pony-buildout()
  "Run buildout again on the current project"
  (interactive)
  (let ((buildout (pony-buildout-cmd))
        (cfg (concat
              (expand-file-name "../"
                                (file-name-directory (pony-buildout-cmd)))
              "buildout.cfg")))
    (if (not (file-exists-p cfg))
        (progn
          (message "couldn't find buildout.cfg")
          (setq cfg nil)))
    (if (and buildout cfg)
        (progn
          (message "Starting buildout... This may take some time")
          (pony-comint-pop
           "buildout" buildout
           (list "-c" cfg))))))

;;;###autoload
(defun pony-buildout-bin()
  "Run a script from the buildout bin/ dir"
  (interactive)
  (let ((buildout (pony-buildout-cmd)))
    (if buildout
        (pony-comint-pop "buildout"
                         (minibuffer-with-setup-hook 'minibuffer-complete
                           (completing-read "bin/: "
                                            (pony-buildout-list-bin)))
                         nil))))

;; Database

;;;###autoload
(defun pony-db-shell ()
  "Run interpreter for this project's default database as an inferior process."
  (interactive)
  (let ((buffer-name "*pony-db-shell*")
        (db-format (if (pony-setting-p "DATABASE_ENGINE") "DATABASE_%s"
                     "DATABASES['default']['%s']")))
    (if (comint-check-proc buffer-name)
        (pop-to-buffer buffer-name)
      (flet ((db-setting (name) (pony-get-setting (format db-format name)))
             ;; Rebind sql-get-login so that sql-product-interactive
             ;; doesn't try to prompt the user.
             (sql-get-login (&rest what)))
        (let* ((db-engine (db-setting "ENGINE"))
               (engine (car (last (split-string db-engine "\\."))))
               (sql-product
                (loop for product in sql-product-alist
                      if (search (symbol-name (car product)) engine)
                      return (car product)
                      finally do
                      (error "Don't know how to connect to %s" engine)))
               (sql-user (db-setting "USER"))
               (sql-password (db-setting "PASSWORD"))
               (sql-database (db-setting "NAME"))
               (sql-server (db-setting "HOST")))
          (save-excursion (sql-product-interactive))
          (when (pony-pop "*SQL*" :dirlocals t)
            (rename-buffer buffer-name t)))))))

;; Fabric

;;;###autoload
(defun pony-fabric-p ()
  "Is this project using fabric?"
  (let ((cmdlist (pony-fabric-list-commands)))
    (if (and (equal "Fatal" (first cmdlist))
             (equal "error:" (second cmdlist)))
        nil
      t)))

;;;###autoload
(defun pony-fabric-list-commands()
  "List of all fabric commands for project as strings"
  (split-string (shell-command-to-string "fab --list | awk '{print $1}'|grep -v Available")))

;;;###autoload
(defun pony-fabric-run(cmd)
  "Run fabric command"
  (pony-comint-pop "fabric" "fab" (list cmd)))

;;;###autoload
(defun pony-fabric()
  "Run a fabric command"
  (interactive)
  (if (pony-fabric-p)
      (pony-fabric-run (minibuffer-with-setup-hook 'minibuffer-complete
                         (completing-read "Fabric: "
                                          (pony-fabric-list-commands)))))
  (message "No fabfile found!"))

;;;###autoload
(defun pony-fabric-deploy()
  "Deploy project with fab deploy"
  (interactive)
  (pony-fabric-run "deploy"))

;; GoTo

(defun pony-template-decorator()
  "Return either the name of a template from within a decorator around a view
or nil. E.G. the following pattern:

@renders_to('some_template.html')
def my_view(request):
    return 'HAI'

"
  (save-excursion
    (progn
      (search-backward-regexp "^def")
      (previous-line)
      (if (looking-at "^@.*['\"]\\([a-z/_.]+html\\).*$")
          (buffer-substring (match-beginning 1) (match-end 1))
        nil))))

(defun pony-convert-string-sequence(python-string)
  "Convert the string representatin of a python list/tuple of strings
to its lisp equivalent"
  (split-string
   (replace-regexp-in-string "[][()'\"]" "" python-string) ", ?"))

(defun pony-find-file-in-path(file path)
  "Find FILE if it exists in one the directories in PATH.

If path doesn't exist in default-directory try to search it in
parent directories.
"
  (dolist (dir path)
    (let ((file-absolute (expand-file-name file (pony-locate dir))))
      (if (file-exists-p file-absolute)
          (return (find-file file-absolute))))))

;;;###autoload
(defun pony-goto-template()
  "Jump-to-template-at-point"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((template
           (if (looking-at "^.*['\"]\\([a-z/_.]+html\\).*$")
               (buffer-substring (match-beginning 1) (match-end 1))
             (pony-template-decorator))))
      (unless (and template
                   (pony-find-file-in-path
                    template (pony-convert-string-sequence
                              (pony-get-setting "TEMPLATE_DIRS"))))
        (message (format "Template %s not found" template))))))

;; TODO
;;;###autoload?
;; (defun pony-reverse-url ()
;;   "Get the URL for this view"
;;   (interactive)
;;   (setq found nil)
;;   (setq view (concat (pony-get-app) ".views." (pony-get-func)))
;;   (message view)
;;   (dolist
;;       (fpath (find-file default-directory "urls.py$"))
;;     (setq mybuffer (get-buffer-create " myTemp"))
;;     (switch-to-buffer mybuffer)
;;     (insert-file-contents fpath)
;;     (search-forward view)))

;;;###autoload
(defun pony-resolve (url)
  "Jump to the view file that URL resolves to

This feature is somewhat experimental and known to break in some cases.

Bug reports welcome. Patches even more so :)"
  (interactive "sUrl: ")
  (let* ((default-directory (pony-project-root))
         (program
          (progn
            (format (concat "python -c '"
                            (mapconcat 'identity
                                       '("import os"
                                         "os.environ[\"DJANGO_SETTINGS_MODULE\"]=\"settings\""
                                         "from django.core.urlresolvers import resolve"
                                         "from django.utils.importlib import import_module"
                                         "f=resolve(\"%s\")[0]"
                                         "path=import_module(f.__module__).__file__"
                                         "path=path[-1]==\"c\" and path[:-1] or path"
                                         "print f.__name__, path")
                                       "; ")
                            "'") url)))
         (output (shell-command-to-string program))
         (fun (first (split-string output)))
         (file (car (last (split-string output)))))
    (find-file file)
    ;; TODO Search for file. This currently doesn't work because of decorators
    ))

;;;###autoload
(defun pony-goto-settings()
  (interactive)
  "Open the settings.py for this project"
  (find-file (pony-get-settings-module)))

;; Manage

(defun pony-list-commands()
  "List of managment commands for the current project"
  (let ((command (concat (pony-active-python) " " (pony-manage-cmd) " --help")))
    (with-temp-buffer
      (insert (shell-command-to-string command))
      (goto-char (point-min))
      (if (looking-at
           "\\(\\(.*\n\\)*Available subcommands:\\)\n\\(\\([^:]*\n\\)+?\\)")
          (split-string (buffer-substring (match-beginning 3) (match-end 3)))
        nil))))

(defun pony-manage-run(args)
  "Run the pony-manage command completed from the minibuffer"
  (pony-manage-pop "ponymanage" (pony-manage-cmd) args))

;;;###autoload
(defun pony-manage()
  "Interactively call the pony manage command.

Second string that is read from minibuffer may be an actual
list of space separated arguments for the previously chosen management
command. If some of the arguments contain space itself they should be quoted
with double quotes like \"...\"."
  (interactive)
  (let* ((command (minibuffer-with-setup-hook 'minibuffer-complete
                              (completing-read "Manage: "
                                               (pony-list-commands))))
         (args (split-string-and-unquote
                (read-from-minibuffer (concat command ": ")))))
    (pony-manage-run (cons command args))))

;;;###autoload
(defun pony-flush()
  "Flush the app"
  (interactive)
  (pony-manage-run (list "flush")))

;; Fixtures

;;;###autoload
(defun pony-dumpdata()
  "Dumpdata to json"
  (interactive)
  (let ((dump (read-from-minibuffer "Dumpdata: " (pony-get-app)))
        (target (pony-mini-file "File: ")))
    (shell-command (concat
                    (pony-active-python) " "
                    (pony-manage-cmd) " dumpdata " dump " > " target))
    (message (concat "Written to " target))))

;;;###autoload
(defun pony-loaddata ()
  "Load a fixture into the current project's dev database"
  (interactive)
  (let ((fixture (pony-mini-file "Fixture: ")))
    (pony-manage-pop "ponymanage" (pony-manage-cmd)
                     (list "loaddata" fixture))
    (insert (concat "Loaded fixture at " fixture))))

;; Server

;;;###autoload
(defun pony-runserver()
  "Start the Django development server.

If the server is currently running, just switch to the buffer.

If you are currently in the *ponyserver* buffer, restart the server"
  (interactive)
  (let* ((buffname "*ponyserver*")
         (proc (get-buffer-process buffname))
         (buff (get-buffer buffname))
         (working-dir default-directory))
    (if proc
       (progn
          (message "Pony Dev Server already running")
          (if (and buff (equalp buff (current-buffer)))
              (pony-restart-server)
            (pony-pop buffname)))
      (pony-startserver))))

(defun pony-startserver ()
  "Start the Django development server.

If the project has django_extras installed and the excellent `runserver_plus'
command is available, use that, otherwise fall back to manage.py runserver."
  (if (pony-command-exists-p "runserver_plus")
      (setq command "runserver_plus")
    (setq command "runserver"))
  (let ((defualt-directory (pony-project-root)))
    (pony-manage-pop "ponyserver" (pony-manage-cmd)
                     (list command
                           (concat pony-server-host ":"  pony-server-port)))))

;;;###autoload
(defun pony-stopserver()
  "Stop the dev server"
  (interactive)
  (let ((proc (get-buffer-process "*ponyserver*")))
    (when proc (kill-process proc t))))

;;;###autoload
(defun pony-restart-server ()
  "Restart the pony Django dev server.
Django extras does this better with the Werkzeug server, but sometimes
you can't have nice things."
  (interactive)
  (pony-stopserver)
  (run-with-timer 1 nil 'pony-startserver))

;;;###autoload
(defun pony-temp-server ()
  "Relatively regularly during development, I need/want to set up a development
server instance either on a nonstandard (or second) port, or that will be accessible
to the outside world for some reason. Meanwhile, i don't want to set my default host to 0.0.0.0
This function allows you to run a server with a 'throwaway' host:port"
  (interactive)
  (let ((args (list "runserver" (read-from-minibuffer "host:port "))))
    (pony-manage-pop "ponytempserver" (pony-manage-cmd)
                     args)))

;; View server

;;;###autoload
(defun pony-browser()
  "Open a tab at the development server"
  (interactive)
  (let ((url (concat "http://" pony-server-host ":"  pony-server-port))
        (proc (get-buffer-process "*ponyserver*")))
    ;; use actual url if process is already running
    (if proc
        (save-excursion
          (progn
            (set-buffer "*ponyserver*")
            (goto-char (point-max))
            (if (search-backward-regexp "\\(Starting development server\\|Development server is running\\) at \\(.+\\)\n")
                (setq url (match-string-no-properties 2)))))
      (pony-runserver))
    (run-with-timer 2 nil 'browse-url url)))

;; Shell

;;;###autoload
(defun pony-shell()
  "Open a Python shell with the current pony project's context loaded.

If the project has the django_extras package installed, then use the excellent
`shell_plus' command. Otherwise, fall back to manage.py shell "
  (interactive)
  (let ((command (if (pony-command-exists-p "shell_plus")
                     "shell_plus" "shell")))
    (pony-manage-pop "ponysh" (pony-manage-cmd) (list command))))

;; Startapp

;;;###autoload
(defun pony-startapp()
  "Run the pony startapp command"
  (interactive)
  (let ((app (read-from-minibuffer "App name: ")))
    (pony-command-if-exists "ponymigrations"
                           "startapp" app)))

;; Syncdb / South

;;;###autoload
(defun pony-syncdb()
  "Run Syncdb on the current project"
  (interactive)
  (pony-manage-pop "ponymigrations" (pony-manage-cmd) (list "syncdb")))

;;;###autoload
(defun pony-south-convert()
  "Convert an existing app to south"
  (interactive)
  (let ((app (read-from-minibuffer "Convert: " (pony-get-app))))
    (pony-manage-popif "ponymigrations" "convert_to_south" (list app))))

;;;###autoload
(defun pony-south-schemamigration()
  "Create migration for modification"
  (interactive)
  (let ((app (read-from-minibuffer "Schema Migration: " (pony-get-app))))
    (pony-manage-popif "ponymigrations" "schemamigration"
                       (list app "--auto"))))

;;;###autoload
(defun pony-south-migrate()
  "Migrate app"
  (interactive)
  (let ((app (read-from-minibuffer "Migrate: " (pony-get-app))))
    (pony-manage-popif "ponymigrations" "migrate" (list app))))

;; (defun pony-south-fake ()
;;   "Fake a migration for a model"
;;   (interactive)
;;   (let ((app (read-from-minibuffer "Convert: " (pony-get-app)))
;;         (migration (read-from-minibuffer "migration: "
;;                                          (pony-south-get-migrations))))
;;     (pony-command-if-exists "ponymigrations"
;;                               "migrate" (list app migrations))))

;;;###autoload
(defun pony-south-initial ()
  "Run the initial south migration for an app"
  (interactive)
  (let ((app (read-from-minibuffer "Initial migration: " (pony-get-app))))
    (pony-manage-popif "ponymigrations" "schemamigration" (list app "--initial"))))

;; CELERY

;;;###autoload
(defun pony-celeryd-start ()
  "Run celeryd"
  (interactive)
  (let* ((command "celeryd"))
    (pony-manage-pop "ponyceleryd" (pony-manage-cmd) (cons command (list)))))

;;;###autoload
(defun pony-celeryd-stop()
  "Stop celeryd"
  (interactive)
  (let ((proc (get-buffer-process "*ponyceleryd*")))
    (when proc (kill-process proc t))))

;;;###autoload
(defun pony-celeryd-restart ()
  "Restart celeryd"
  (interactive)
  (pony-celeryd-stop)
  (run-with-timer 1 nil 'pony-celeryd-start))

;; TAGS

;;;###autoload
(defun pony-tags()
  "Generate new tags table"
  (interactive)
  (let* ((tags-dir (read-directory-name "TAGS location: "
                                       (pony-project-root)))
         (default-directory tags-dir))
    (message "TAGging... this could take some time")
    (shell-command pony-etags-command )
    (visit-tags-table (concat tags-dir "TAGS"))
    (message "TAGS table regenerated")))

;; Testing

;;;###autoload
(defun pony-test (command)
  "Run the test(s) given by `command'."
  (interactive
   (let* ((defuns (subseq (split-string (which-function) "\\.") 0 2))
          (class (first defuns))
          (func (let ((f (second defuns))) (and f (string-match "^test" f) f)))
          (module (pony-get-module))
          (default-command
            (concat module (and module class ".") class (and class func ".") func)))
     (list (read-string "Test: " default-command))))
  (let ((buffer (get-buffer "*ponytests*")))
    (when buffer
      (save-excursion
        (pop-to-buffer buffer)
        (erase-buffer))))
  (pony-manage-pop "ponytests" (pony-manage-cmd)
                   (list "test" (if pony-test-failfast "--failfast" "") command))
  (pony-test-mode))

;;;###autoload
(defun pony-test-open ()
  "Open the file in a traceback at the line specified"
  (interactive)
  (move-beginning-of-line nil)
  (if (looking-at ".*File \"\\([a-z/_]+.py\\)\", line \\([0-9]+\\)")
      (let ((file (buffer-substring (match-beginning 1) (match-end 1)))
            (line (buffer-substring (match-beginning 2) (match-end 2))))
        (find-file-other-window file)
        (goto-line (string-to-number line)))
    (message "failed")))

;;;###autoload
(defun pony-test-goto-err()
  "Go to the file and line of the last stack trace in a test buffer"
  (interactive)
  (goto-char (search-backward "File"))
  (pony-test-open))

;;;###autoload
(defun pony-test-up()
  "Move up the traceback one level"
  (interactive)
  (search-backward-regexp "File \"\\([a-z_/]+.py\\)\"" nil t))

;;;###autoload
(defun pony-test-down()
  "Move up the traceback one level"
  (interactive)
  (search-forward-regexp "File \"\\([a-z_/]+.py\\)\"" nil t))

;;;###autoload
(defun pony-test-hl-files ()
  "Highlight instances of Files in Test buffers"
  (hi-lock-face-buffer "File \"\\([a-z/_]+.py\\)\", line \\([0-9]+\\)"
                       'hi-blue))
;; Modes ;;

;; Snippets

;;;###autoload
(defun pony-load-snippets()
  "Load snippets if yasnippet installed and pony-snippet-dir is set"
  (interactive)
  (when pony-snippet-dir
    (cond
     ((fboundp 'yas-load-directory)
      (yas-load-directory pony-snippet-dir))
     ((fboundp 'yas/load-directory)
      (yas/load-directory pony-snippet-dir)))))

;; Keymaps

(defun pony-key(binding function)
  "Bind function to binding in pony-minor-mode-map"
  (define-key pony-minor-mode-map binding function))

(defun ponyt-key (binding function)
  "Bind for test mode. Hacky as hell"
  (define-key pony-test-minor-mode-map binding function))

(defvar pony-minor-mode-map
  (let ((map (make-keymap)))
    map))
(pony-key "\C-c\C-pb" 'pony-browser)
(pony-key "\C-c\C-pd" 'pony-db-shell)
(pony-key "\C-c\C-pf" 'pony-fabric)
(pony-key "\C-c\C-pgt" 'pony-goto-template)
(pony-key "\C-c\C-pgs" 'pony-goto-settings)
(pony-key "\C-c\C-pr" 'pony-runserver)
(pony-key "\C-c\C-pm" 'pony-manage)
(pony-key "\C-c\C-ps" 'pony-shell)
(pony-key "\C-c\C-p!" 'pony-shell)
(pony-key "\C-c\C-pt" 'pony-test)
(pony-key "\C-c\C-p\C-r" 'pony-reload-mode)
(pony-key "\C-c\C-p\c" 'pony-celeryd-start)

(defvar pony-test-minor-mode-map
  (let ((map (make-keymap)))
    map))

(ponyt-key "\C-c\C-g" 'pony-test-goto-err)
(ponyt-key "\C-p" 'pony-test-up)
(ponyt-key "\C-n" 'pony-test-down)
(ponyt-key (kbd "M-RET") 'pony-test-open)

;; Menu
;;
;; This will only work OOTB for emacs >=19 due to the dependency on easymenu
;; but frankly, that's fine with me.
(defvar pony-menu nil
  "The menu for Pony mode.")
(and (require 'easymenu)
     (easy-menu-define
       pony-menu pony-minor-mode-map "Pony Mode Menu"
       '("Pony"
         ;; "Interactive"
          ["Launch Pony shell" pony-shell
           :help " `pony-shell'

Open a Python shell with the current pony project's context loaded\.

If the project has the django_extras package installed, then use the excellent
`shell_plus' command\. Otherwise, fall back to manage\.py shell . "]

          ["Launch Pony db shell" pony-db-shell
           :help " `pony-db-shell'

Run interpreter for this project's default database as an inferior process\.. "]

          "-"
          ;; Server

          ["Pony runserver" pony-runserver
           :help " `pony-runserver'

Start the Django development server\.

If the server is currently running, just switch to the buffer\.

If you are currently in the \*ponyserver\* buffer, restart the server. "]

          ["Pony stopserver" pony-stopserver
           :help " `pony-stopserver'

Stop the dev server. "]

          ["Pony restart server" pony-restart-server
           :help " `pony-restart-server'

Restart the pony Django dev server\.
Django extras does this better with the Werkzeug server, but sometimes
you can't have nice things\.. "]

          ["Pony browser" pony-browser
           :help " `pony-browser'

Open a tab at the development server. "]


         "-"
         ("Models"

          ["Pony syncdb" pony-syncdb
           :help " `pony-syncdb'

Run Syncdb on the current project. "]

          ["Pony south convert" pony-south-convert
           :help " `pony-south-convert'

Convert an existing app to south. "]

          ["Pony south schemamigration" pony-south-schemamigration
           :help " `pony-south-schemamigration'

Create migration for modification. "]

          ["Pony south migrate" pony-south-migrate
           :help " `pony-south-migrate'

Migrate app. "]

          ;; Management
          "-"

          ["Pony manage" pony-manage
           :help " `pony-manage'

Interactively call the pony manage command\.

Second string that is read from minibuffer may be an actual
list of space separated arguments for the previously chosen management
command\. If some of the arguments contain space itself they should be quoted
with double quotes like "\.\.\."\.. "]

          ["Pony dumpdata" pony-dumpdata
           :help " `pony-dumpdata'

Dumpdata to json. "]

          ["Pony flush" pony-flush
           :help " `pony-flush'

Flush the app. "]

          ["Pony startapp" pony-startapp
           :help " `pony-startapp'

Run the pony startapp command. "]

          ;; Tests
          "-"

          ["Pony test" pony-test
           :help " `pony-test'

Run the test(s) given by `command'\.. "]

          ;; Goto
          "-"

          ["Pony resolve" pony-resolve
           :help " `pony-resolve'

Jump to the view file that URL resolves to

This feature is somewhat experimental and known to break in some cases\.

Bug reports welcome\. Patches even more so :). "]

          ["Pony goto settings" pony-goto-settings
           :help " `pony-goto-settings'

. "]

          ["Pony goto template" pony-goto-template
           :help " `pony-goto-template'

Jump-to-template-at-point. "]

          "-"

          ["Pony setting" pony-setting
           :help " `pony-setting'

Interactively display a setting value in the minibuffer. "]
          )
         "-"
         ("Environment"

          ["Pony tags" pony-tags
           :help " `pony-tags'

Generate new tags table. "]

          "-"

          ["Pony buildout" pony-buildout
           :help " `pony-buildout'

Run buildout again on the current project. "]

          ["Pony buildout bin" pony-buildout-bin
           :help " `pony-buildout-bin'

Run a script from the buildout bin/ dir. "]

          "-"

          ["Pony fabric" pony-fabric
           :help " `pony-fabric'

Run a fabric command. "]

          ["Pony fabric deploy" pony-fabric-deploy
           :help " `pony-fabric-deploy'

Deploy project with fab deploy. "]

          ))))

;; Pony-minor-mode
(defvar pony-minor-mode-hook nil)

(define-minor-mode pony-minor-mode
  "Ponyriffic"
  :initial nil
  :lighter " Pony"
  :keymap pony-minor-mode-map)

(defun pony-mode()
  "Initialize Pony mode"
  (interactive)
  (pony-minor-mode t)
  (run-hooks 'pony-minor-mode-hook)
  (pony-load-snippets))

;;;###autoload
(defun pony-mode-disable ()
  "Turn off pony-mode in this buffer"
  (interactive)
  (pony-minor-mode))

;;; ###pony-tmpl
(load-file (concat (file-name-directory (or load-file-name default-directory)) "/pony-tpl.el"))

(define-minor-mode pony-test-minor-mode
  "Pony Testin'"
  :initial nil
  :lighter " DT"
  :keymap pony-test-minor-mode-map)

(defun pony-test-mode ()
  "Enable Pony test minor mode"
  (interactive)
  (pony-test-minor-mode t)
  (pony-test-hl-files))

;; Hooks

(add-hook 'python-mode-hook
          (lambda ()
            (if (pony-project-root)
                (pony-mode))))

(add-hook 'html-mode-hook
           (lambda ()
             (if (pony-project-root)
                 (if pony-enable-template-mode
                       (pony-tpl-mode)))))

(add-hook 'dired-mode-hook
          (lambda ()
            (if (pony-project-root)
                (pony-mode))))

(provide 'pony-mode)
;;; pony-mode.el ends here
