;; Lisp
(defun chomp (str)
  "Chomp leading and tailing whitespace www.emacswiki.org/emacs/ElispCookbook"
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

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
    (insert default-directory)
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

(defun django-manage()
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
  (if (string-match cmd (shell-command-to-string (django-manage)))
      (setq found-command t)
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
          (setq value (chomp (shell-command-to-string python-c))))
      nil)))

(defun django-setting()
  "Interactively display a setting value in the minibuffer"
  (interactive)
  (let ((setting (read-from-minibuffer "Get setting: " (word-at-point))))
    (message (concat setting " : " (django-get-setting setting)))))

;; Fabric
(defun django-fabric-deploy()
  "Deploy project with fab deploy"
  (interactive)
  (start-process "fabric" "*fabric*" "fab" "deploy")
  (pop-to-buffer (get-buffer "*fabric*")))

;; Server
(defun django-runserver()
  "Start the dev server"
  (interactive)
  (let ((proc (get-buffer-process "*djangoserver*")))
    (if proc
        (message "Django Dev Server already running")
      (if (django-command-exists "runserver_plus")
          (setq command "runserver_plus")
        (setq command "runserver"))
      (start-process "djangoserver" "*djangoserver*"
                     (django-manage)
                     command)))
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
  (apply 'make-comint "djangosh" (django-manage) nil (list command))
  (pop-to-buffer (get-buffer "*djangosh*")))


;; Syncdb
(defun django-syncdb()
  "Run Syncdb on the current project"
  (interactive)
  (start-process "djangomigrations" "*djangomigrations*"
                 (django-manage) "syncdb")
  (pop-to-buffer (get-buffer "*djangomigrations*")))

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
          (start-process "djangotests" "*tests*"
                         (django-manage)
                         "test"
                         confirmed-command)
          (pop-to-buffer (get-buffer "*tests*"))))))

;; Keymaps

(defvar django-minor-mode-map
  (let ((map (make-keymap)))
    map))
(define-key django-minor-mode-map "\C-c\C-db" 'django-browser)
(define-key django-minor-mode-map "\C-c\C-dfd" 'django-fabric-deploy)
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
    (define-key menu-map [runserver]
      '("Run dev server for project" . django-runserver))
    (define-key menu-map [deploy]
      '("Run fabric deploy function" . django-fabric-deploy))
    (define-key menu-map [shell]
      '("Launch Django shell" . django-shell))
    (define-key menu-map [test]
      '("Run tests" . django-test))
    (define-key menu-map [setting]
      '("Check setting value for project" . django-setting)))


;; Minor mode
(define-minor-mode django-minor-mode
  "Djangoriffic"
  :initial nil
  :lighter " Django"
  :keymap django-minor-mode-map)

;; Hooks

(add-hook 'python-mode-hook
          (lambda ()
            (if (django-project-root)
                (django-minor-mode t))))
