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

(defcustom django-server-host "localhost"
  "Host to run django dev server"
  :group 'django
  :type 'string)

(defcustom django-server-port "8000"
  "Port to run django dev server"
  :group 'django
  :type 'string)

;; Dependancies and environment sniffing
(require 'sgml-mode)

;; Lisp
(defun chomp (str)
  "Chomp leading and tailing whitespace www.emacswiki.org/emacs/ElispCookbook"
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun django-pop(buffer)
  "Wrap pop-to and get buffer"
  (pop-to-buffer (get-buffer buffer)))

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
  (let (
        (django (concat (django-project-root) "../bin/django"))
        (manage (concat (django-project-root) "manage.py")))
    (if (file-exists-p django)
        (setq found django)
      (if (and (not found) (file-exists-p manage))
          (setq found manage)
        nil))
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
  (start-process "fabric" "*fabric*" "fab" cmd)
  (django-pop "*fabric*"))

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
    (shell-command (concat (django-manage-cmd) " dumpdata " dump " > " target))
  (message (concat "Written to " target))))

;; GoTo
(defun django-goto-template()
  "Jump-to-template-at-point"
  (interactive)
  (message (replace-regexp-in-string
            "^.*['\"]\\(:?.*.html\\)" "" (thing-at-point 'line)))
  )

;; Server

(defun django-runserver()
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
        (start-process "djangoserver" "*djangoserver*"
                       (django-manage-cmd)
                       command
                       (concat django-server-host ":"  django-server-port))
        (cd working-dir))))
  (pop-to-buffer (get-buffer "*djangoserver*")))

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
  (apply 'make-comint "djangosh" (django-manage-cmd) nil (list command))
  (pop-to-buffer (get-buffer "*djangosh*")))

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
  (pop-to-buffer (get-buffer "*djangomigrations*")))

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
          (pop-to-buffer (get-buffer "*djangomigrations*")))
      (message "South doesn't seem to be installed"))))

(defun django-south-migrate()
  "Migrate app"
  (interactive)
  (let ((app (read-from-minibuffer "Convert: " (django-get-app))))
    (django-command-if-exists "djangomigrations"
                              "migrate" app)))


;; Testing
(defun django-test()
  "Run tests here"
  (interactive)
  (let ((func (django-get-func))
        (class (django-get-class))
        (app (django-get-app))
        (command nil))
    (if (and func class app (string= "test" (substring func 0 4)))
        (setq command (concat app "." class "." func))
      (if (and class app)
          (setq command (concat app "." class))
        (if app
            (setq command app))))
    (if command
        (let ((confirmed-command
               (read-from-minibuffer "test: " command)))
          (apply 'make-comint "djangotests" (django-manage-cmd) nil
                 (list "test" confirmed-command))
          (pop-to-buffer (get-buffer "*djangotests*"))))))

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

(defvar django-minor-mode-map
  (let ((map (make-keymap)))
    map))
(define-key django-minor-mode-map "\C-c\C-db" 'django-browser)
(define-key django-minor-mode-map "\C-c\C-dd" 'django-db-shell)
(define-key django-minor-mode-map "\C-c\C-df" 'django-fabric)
(define-key django-minor-mode-map "\C-c\C-dm" 'django-syncdb)
(define-key django-minor-mode-map "\C-c\C-dr" 'django-runserver)
(define-key django-minor-mode-map "\C-c\C-ds" 'django-shell)
(define-key django-minor-mode-map "\C-c\C-dt" 'django-test)
(define-key django-minor-mode-map "\C-c\C-d\C-r" 'django-reload-mode)

;; Menu
(let ((menu-map (make-sparse-keymap "Django")))
    (define-key django-minor-mode-map [menu-bar django] (cons "Django " menu-map))
    (define-key menu-map [browser]
      '("Launch project in browser" . django-browser))
    (define-key menu-map [dbshell]
      '("Launch Django db shell" . django-db-shell))
    (define-key menu-map [fabric]
      '("Run fabric function" . django-fabric))
    (define-key menu-map [deploy]
      '("Run fabric 'deploy' function" . django-fabric-deploy))
    (define-key menu-map [syncdb]
      '("Syncdb" . django-syncdb))
    (define-key menu-map [south-convert]
      '("South convert" . django-south-convert))
    (define-key menu-map [south-schemamigration]
      '("South Schemamigration --auto" . django-south-schemamigration))
    (define-key menu-map [south-migrate]
      '("South migrate" . django-south-migrate))
    (define-key menu-map [startapp]
      '("South migrate" . django-startapp))
    (define-key menu-map [runserver]
      '("Run dev server for project" . django-runserver))
    (define-key menu-map [shell]
      '("Launch Django shell" . django-shell))
    (define-key menu-map [test]
      '("Run tests" . django-test))
    (define-key menu-map [setting]
      '("Check setting value for project" . django-setting))
    (define-key menu-map [dumpdata]
      '("Dumpdata to json" . django-dumpdata)))


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
    '("{% ?\\(\\(end\\)?\\(extends\\|for\\|cycle\\|filter\\|firstof\\|debug\\|if\\(changed\\|equal\\|notequal\\|\\)\\|include\\|load\\|now\\|regroup\\|spaceless\\|ssi\\|templatetag\\|widthratio\\|block\\|trans\\)\\) ?.*? ?%}" . 1)
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
