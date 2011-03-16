;; Copyright (C) 2011 David Miller <david@deadpansincerity.com>

;; Authors: David Miller <david@deadpansincerity.com>

;; Keywords: python django

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
(defgroup django nil
  "Djangification for Emacs"
  :group 'programming
  :prefix "django-")

(defcustom django-etags-command "find . | grep .py | xargs etags"
  "Command to generate tags table for project"
  :group 'django
  :type 'string)

(defcustom django-server-host "localhost"
  "Host to run django dev server"
  :group 'django
  :type 'string)

(defcustom django-server-port "8000"
  "Port to run django dev server"
  :group 'django
  :type 'string)

(defcustom django-test-failfast t
  "Run django tests with failfast?"
  :group 'django
  :type 'bool)

;; Dependancies and environment sniffing
(require 'sgml-mode)

;; Lisp
(defun chomp (str)
  "Chomp leading and tailing whitespace www.emacswiki.org/emacs/ElispCookbook"
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun django-pop(buffer)
  "Wrap pop-to and get buffer"
  (pop-to-buffer (get-buffer buffer))
  (django-mode))

(defun django-comint-pop(name command args)
  "Make a comint buffer and pop to it."
  (ansi-color-for-comint-mode-on)
  (apply 'make-comint name command nil args)
  (django-pop (concat "*" name "*"))
  (django-mode))

(defun django-dir-excursion(dir &rest rest)
  "django-comint-pop where we need to change into `dir` first"
  (let ((curdir default-directory))
     (cd dir)
     (apply 'django-comint-pop rest)
     (cd curdir)))

;; Django-mode
(defun django-reload-mode()
  (interactive)
  (load-library "django"))

;; Python
(defun django-get-func()
  "Get the function currently at point - depends on python-mode"
  (save-excursion
    (if (py-go-up-tree-to-keyword "\\(def\\)")
        (if (looking-at "[ \t]*[a-z]+[\s]\\([a-z_]+\\)\\>")
            (buffer-substring (match-beginning 1) (match-end 1))
          nil))))

(defun django-get-class()
  "Get the class at point - depends on python-mode"
  (save-excursion
    (if (py-go-up-tree-to-keyword "\\(class\\)")
        (if (looking-at "[ \t]*[a-z]+[\s]\\([a-zA-Z]+\\)\\>")
            (buffer-substring (match-beginning 1) (match-end 1))
          nil))))

(defun django-get-app()
  "Get the name of the django app currently being edited"
  (with-temp-buffer
    (insert (expand-file-name default-directory))
    (goto-char (point-min))
    (if (looking-at (concat (django-project-root) "\\([a-z]+\\).*"))
        (buffer-substring (match-beginning 1) (match-end 1))
      nil)))

;; Environment
(defun django-project-root()
  "Return the root of the project(dir with manage.py in) or nil"
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
    (if found (expand-file-name curdir))))

(defun django-manage-cmd()
  "Return the current manage command"
  (let ((found nil)
        (buildout (concat (django-project-root) "bin/django"))
        (django (concat (django-project-root) "../bin/django"))
        (manage (concat (django-project-root) "manage.py")))
    (if (file-exists-p buildout)
        (setq found buildout)
      (if (file-exists-p django)
          (setq found django)
        (if (and (not found) (file-exists-p manage))
            (setq found manage)
          nil)))
    (if found (expand-file-name found))))

(defun django-command-exists(cmd)
  "Is cmd installed in this app"
  (if (string-match cmd (shell-command-to-string (django-manage-cmd)))
      (setq found-command t)
    nil))

(defun django-command-if-exists(proc-name command args)
  "Run `command` if it exists"
  (if (django-command-exists command)
      (let ((process-buffer (concat "*" proc-name "*")))
        (progn
          (message (concat command " " args))
          (start-process proc-name process-buffer
                         (django-manage-cmd)
                         command args)
          (pop-to-buffer (get-buffer process-buffer))))

    nil))

(defun django-get-setting(setting)
  "Get the django settings.py value for `setting`"
  (let ((working-dir default-directory)
        (curdir default-directory)
        (max 10)
        (found nil)
        (value nil))
    (while (and (not found) (> max 0))
      (progn
        (if (file-exists-p (concat curdir "settings.py"))
            (progn
              (setq found t))
          (progn
            (setq curdir (concat curdir "../"))
            (setq max (- max 1))))))

    (if found
        (let ((settings-dir (expand-file-name curdir))
              (python-c (concat "python -c 'import settings; print settings.'"
                                setting)))
          (cd settings-dir)
          (setq value (chomp (shell-command-to-string python-c)))
          (cd working-dir))
      nil)
    (if value (format "%s" value) nil)))

(defun django-setting()
  "Interactively display a setting value in the minibuffer"
  (interactive)
  (let ((setting (read-from-minibuffer "Get setting: " (word-at-point))))
    (message (concat setting " : " (django-get-setting setting)))))

;; Buildout
(defun django-buildout-cmd()
  "Return the buildout command or nil if we're not in a buildout"
  (let ((project-root (django-project-root))
        (buildouts (list "bin/buildout" "../bin/buildout"))
        (found nil))
    (loop for loc in buildouts
          do
          (if (file-exists-p (expand-file-name (concat project-root loc)))
            (setq found (concat project-root loc))))
    (if found
        found
      nil)))

(defun django-buildout-list-bin()
  "List the commands available in the buildout bin dir"
  (directory-files (file-name-directory (django-buildout-cmd))))

(defun django-buildout()
  "Run buildout again on the current project"
  (interactive)
  (let ((buildout (django-buildout-cmd)))
    (if buildout
        (django-dir-excursion
         (django-project-root) "buildout" buildout nil))))

(defun django-buildout-bin()
  "Run a script from the buildout bin/ dir"
  (interactive)
  (let ((buildout (django-buildout-cmd)))
    (if buildout
        (django-comint-pop "buildout"
                           (minibuffer-with-setup-hook 'minibuffer-complete
                             (completing-read "bin/: "
                                              (django-buildout-list-bin)))
                           nil))))

;; Database
(defstruct django-db-settings engine name user pass host)

(defun django-get-db-settings()
  "Get Django's database settings"
  (let ((db-settings
         (make-django-db-settings
          :engine (django-get-setting "DATABASE_ENGINE")
          :name (django-get-setting "DATABASE_NAME")
          :user (django-get-setting "DATABASE_USER")
          :pass (django-get-setting "DATABASE_PASSWORD")
          :host (django-get-setting "DATABASE_HOST"))))
    db-settings))

(defun django-db-shell()
  "Run sql-XXX for this project"
  (interactive)
  (let ((db (django-get-db-settings)))
    (progn
      (setq sql-user (django-db-settings-user db))
      (setq sql-password (django-db-settings-pass db))
      (setq sql-database (django-db-settings-name db))
      (setq sql-server (django-db-settings-host db))
      (if (equalp (django-db-settings-engine db) "mysql")
          (sql-connect-mysql)
        (if (equalp (django-db-settings-engine db) "sqlite3")
            (sql-connect-sqlite)
          (if (equalp (substr (django-db-settings-engine db) -9 -1)
                      "psycopg2")
              (sql-connect-postgres))))
      (django-pop "*SQL*")
      (rename-buffer "*DjangoDbShell*"))))


;; Fabric
(defun django-fabric-list-commands()
  "List of all fabric commands for project as strings"
  (split-string (shell-command-to-string "fab --list | awk '{print $1}'|grep -v Available")))

(defun django-fabric-run(cmd)
  "Run fabric command"
  (django-comint-pop "fabric" "fab" (list cmd)))

(defun django-fabric()
  "Run a fabric command"
  (interactive)
  (django-fabric-run (minibuffer-with-setup-hook 'minibuffer-complete
                       (completing-read "Fabric: "
                                        (django-fabric-list-commands)))))

(defun django-fabric-deploy()
  "Deploy project with fab deploy"
  (interactive)
  (django-fabric-run "deploy"))

;; Fixtures
(defun django-dumpdata()
  "Dumpdata to json"
  (interactive)
  (let ((dump (read-from-minibuffer "Dumpdata: " (django-get-app)))
        (target (expand-file-name (read-file-name
                 "File: "
                 (expand-file-name default-directory)))))
    (shell-command (concat
                    (django-manage-cmd) " dumpdata " dump " > " target))
  (message (concat "Written to " target))))

;; GoTo
(defun django-goto-template()
  "Jump-to-template-at-point"
  (interactive)
  (message (replace-regexp-in-string
            "^.*['\"]\\(:?.*.html\\)" "bye" (thing-at-point 'line))))

;; Manage
(defun django-list-commands()
  "List of managment commands for the current project"
  (interactive)
  (with-temp-buffer
    (insert (shell-command-to-string (django-manage-cmd)))
    (goto-char (point-min))
    (if (looking-at
         "\\(\\(.*\n\\)*Available subcommands:\\)\n\\(\\(.*\n\\)+?\\)Usage:")
        (split-string (buffer-substring (match-beginning 3) (match-end 3)))
      nil)))

(defun django-manage-run(args)
  "Run the django-manage command completed from the minibuffer"
  (django-comint-pop "djangomanage" (django-manage-cmd) args))

(defun django-manage()
  "Interactively call the django manage command"
  (interactive)
  (let ((command (minibuffer-with-setup-hook 'minibuffer-complete
                              (completing-read "Manage: "
                                               (django-list-commands)))))
    (django-manage-run (list command
                             (read-from-minibuffer (concat command ": "))))))

;; Server
(defun runserver()
  "Start the dev server"
  (interactive)
  (let ((proc (get-buffer-process "*djangoserver*"))
        (working-dir default-directory))
    (if proc
        (message "Django Dev Server already running")
      (if (django-command-exists "runserver_plus")
          (setq command "runserver_plus")
        (setq command "runserver"))
      (progn
        (cd (django-project-root))
        (django-comint-pop "djangoserver" (django-manage-cmd)
               (list command
                     (concat django-server-host ":"  django-server-port)))
        (cd working-dir)))))


(defun django-stopserver()
  "Stop the dev server"
  (interactive)
  (let ((proc (get-buffer-process "*djangoserver*")))
    (when proc (kill-process proc t))))

;; View server
(defun django-browser()
  "Open a tab at the development server"
  (interactive)
  (let ((url "http://localhost:8000")
        (proc (get-buffer-process "*djangoserver*")))
    (if (not proc)
        (progn
          (django-runserver)
          (sit-for 2)))
    (browse-url url)))

;; Shell
(defun django-shell()
  "Open a shell with the current django project's context loaded"
  (interactive)
  (if (django-command-exists "shell_plus")
      (setq command "shell_plus")
    (setq command "shell"))
  (django-comint-pop "djangosh" (django-manage-cmd) (list command)))

;; Startapp
(defun django-startapp()
  "Run the django startapp command"
  (interactive)
  (let ((app (read-from-minibuffer "App name: ")))
    (django-command-if-exists "djangomigrations"
                           "startapp" app)))

;; Syncdb / South
(defun django-syncdb()
  "Run Syncdb on the current project"
  (interactive)
  (start-process "djangomigrations" "*djangomigrations*"
                 (django-manage-cmd) "syncdb")
  (django-pop "*djangomigrations*"))

(defun django-south-convert()
  "Convert an existing app to south"
  (interactive)
  (let ((app (read-from-minibuffer "Convert: " (django-get-app))))
    (django-command-if-exists "djangomigrations"
                              "convert_to_south" app)))

(defun django-south-schemamigration()
  "Create migration for modification"
  (interactive)
  (let ((app (read-from-minibuffer "Migrate: " (django-get-app))))
    (if (django-command-exists "schemamigration")
        (progn
          (start-process "djangomigrations" "*djangomigrations*"
                         (django-manage-cmd)
                         "schemamigration" app "--auto")
          (django-pop "*djangomigrations*"))
      (message "South doesn't seem to be installed"))))

(defun django-south-migrate()
  "Migrate app"
  (interactive)
  (let ((app (read-from-minibuffer "Convert: " (django-get-app))))
    (django-command-if-exists "djangomigrations"
                              "migrate" app)))

;; TAGS
(defun django-tags()
  "Generate new tags table"
  (interactive)
  (let ((working-dir default-directory)
        (tags-dir (read-directory-name "TAGS location: "
                                       (django-project-root))))
    (cd (expand-file-name tags-dir))
    (message "TAGging... this could take some time")
    (shell-command django-etags-command )
    (visit-tags-table (concat tags-dir "TAGS"))
    (cd working-dir)
    (message "TAGS table regenerated")))

;; Testing
(defun django-test()
  "Run tests here"
  (interactive)
  (let ((func (django-get-func))
        (class (django-get-class))
        (app (django-get-app))
        (command nil)
        (failfast (if django-test-failfast
                      "--failfast"
                    "")))
    (if (and func class app (string= "test" (substring func 0 4)))
        (setq command (concat app "." class "." func))
      (if (and class app)
          (setq command (concat app "." class))
        (if app
            (setq command app))))
    (if command
        (let ((confirmed-command
               (read-from-minibuffer "test: " command)))
          (django-comint-pop "djangotests" (django-manage-cmd)
                 (list "test" failfast confirmed-command))
          (django-test-extra-keys)))))

(defun django-test-goto-err()
  "Go to the file and line of the last stack trace in a test buffer"
  (interactive)
  (goto-char (search-backward "File"))
  (if (looking-at "File \"\\([a-z/]+.py\\)\", line \\([0-9]+\\)")
      (let ((file (buffer-substring (match-beginning 1) (match-end 1)))
            (line (buffer-substring (match-beginning 2) (match-end 2))))
        (find-file-other-window file)
        (goto-line (string-to-number line)))
    (message "failed")))


;; Modes ;;

;; Snippets

(defvar django-snippet-dir (expand-file-name
                            (concat (file-name-directory load-file-name)
                                    "/snippets")))
(defun django-load-snippets()
  "Load snippets if yasnippet installed"
  (interactive)
  (if (fboundp 'yas/load-directory)
      (yas/load-directory django-snippet-dir)))

;; Django-minor-mode

(defvar django-minor-mode-hook nil)

;; Keymaps

(defun django-key(binding function)
  "Bind function to binding in django-minor-mode-map"
  (define-key django-minor-mode-map binding function))

(defvar django-minor-mode-map
  (let ((map (make-keymap)))
    map))
(django-key "\C-c\C-db" 'django-browser)
(django-key "\C-c\C-dd" 'django-db-shell)
(django-key "\C-c\C-df" 'django-fabric)
(django-key "\C-c\C-dr" 'django-runserver)
(django-key "\C-c\C-dm" 'django-manage)
(django-key "\C-c\C-ds" 'django-shell)
(django-key "\C-c\C-dt" 'django-test)
(django-key "\C-c\C-d\C-r" 'django-reload-mode)

(defun django-test-extra-keys()
  "Extra keys for working with test buffers"
  (django-key "\C-c\C-g" 'django-test-goto-err))

;; Menu
(let ((menu-map (make-sparse-keymap "Django")))
    (define-key django-minor-mode-map [menu-bar django] (cons "Django " menu-map))
    (define-key menu-map [browser]
      '("Launch project in browser" . django-browser))
    (define-key menu-map [buildout]
      '("Run buildout on project" . django-buildout))
    (define-key menu-map [buildout-bin]
      '("Run a script from buildout's bin/" . django-buildout-bin))
    (define-key menu-map [dbshell]
      '("Launch Django db shell" . django-db-shell))
    (define-key menu-map [dumpdata]
      '("Dumpdata to json" . django-dumpdata))
    (define-key menu-map [fabric]
      '("Run fabric function" . django-fabric))
    (define-key menu-map [deploy]
      '("Run fabric 'deploy' function" . django-fabric-deploy))
    (define-key menu-map [manage]
      '("Run a management command" . django-manage))
    (define-key menu-map [runserver]
      '("Run dev server for project" . django-runserver))
    (define-key menu-map [stopserver]
      '("Run dev server for project" . django-stopserver))
    (define-key menu-map [setting]
      '("Check setting value for project" . django-setting))
    (define-key menu-map [shell]
      '("Launch Django shell" . django-shell))
    (define-key menu-map [south-convert]
      '("South convert" . django-south-convert))
    (define-key menu-map [south-schemamigration]
      '("South Schemamigration --auto" . django-south-schemamigration))
    (define-key menu-map [south-migrate]
      '("South migrate" . django-south-migrate))
    (define-key menu-map [startapp]
      '("South migrate" . django-startapp))
    (define-key menu-map [syncdb] '("Syncdb" . django-syncdb))
    (define-key menu-map [tags] '("Generate TAGS file" . django-tags))
    (define-key menu-map [test] '("Run tests" . django-test))
)


;; Python Minor mode
(define-minor-mode django-minor-mode
  "Djangoriffic"
  :initial nil
  :lighter " Django"
  :keymap django-minor-mode-map)

(defun django-mode()
  "Initialize Django mode"
  (interactive)
  (django-minor-mode t)
  (run-hooks 'django-minor-mode-hook)
  (django-load-snippets))

;; Django-tpl-minor-mode

(defvar django-tpl-mode-hook nil)

(defconst django-tpl-font-lock-keywords
  (append
   sgml-font-lock-keywords
   (list
    '("{%.*\\(\\bor\\b\\).*%}" . (1 font-lock-builtin-face))

    '("{% ?comment ?%}\\(\n?.*?\\)+?{% ?endcomment ?%}\\|<!--\\(\n?.*?\\)+?-->" . font-lock-comment-face)
    '("{% ?\\(\\(end\\)?\\(extends\\|for\\|cache\\|cycle\\|filter\\|firstof\\|debug\\|if\\(changed\\|equal\\|notequal\\|\\)\\|include\\|load\\|now\\|regroup\\|spaceless\\|ssi\\|templatetag\\|widthratio\\|block\\|trans\\)\\) ?.*? ?%}" . 1)
    '("{{ ?\\(.*?\\) ?}}" . (1 font-lock-variable-name-face))
    '("{%\\|\\%}\\|{{\\|}}" . font-lock-builtin-face)
    ))
  "Highlighting for django-tpl-mode")

(define-minor-mode django-tpl-minor-mode
  "Django-templatin-riffic"
  :initial nil
  :lighter " DjangoTpl"
  :keymap django-minor-mode-map)

(defun django-tpl-mode()
  "Minor mode for editing django templates"
  (interactive)
  (django-tpl-minor-mode t)
  (run-hooks 'django-tpl-mode-hook)
  (set (make-local-variable 'font-lock-defaults)
       '(django-tpl-font-lock-keywords))
  (django-load-snippets))

;; Hooks

(add-hook 'python-mode-hook
          (lambda ()
            (if (django-project-root)
                (django-mode))))

(add-hook 'html-mode-hook
          (lambda ()
            (if (django-project-root)
                  (django-tpl-mode))))
