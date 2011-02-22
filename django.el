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
;; Broken
;; (defun django-shell()
;;   "Open a shell with the current django project's context loaded"
;;   (interactive)
;;   (if (django-command-exists "shell_plus")
;;       (setq command "shell_plus")
;;     (setq command "shell"))
;;   (start-process "djangoshell" "*djangoshell*" (django-manage) command)
;;   (pop-to-buffer (get-buffer "*djangoshell*")))

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
(define-key django-minor-mode-map "\C-c\C-dr" 'django-runserver)
(define-key django-minor-mode-map "\C-c\C-ds" 'django-syncdb)
(define-key django-minor-mode-map "\C-c\C-dt" 'django-test)
(define-key django-minor-mode-map "\C-c\C-d\C-r" 'django-reload-mode)

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
