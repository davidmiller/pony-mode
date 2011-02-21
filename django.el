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
  (let ((found nil)
        (django (concat (django-project-root) "../bin/django"))
        (manage (concat (django-project-root) "manage.py")))
    (if (file-exists-p django)
        (setq found django)
      (if (and (not found) (file-exists-p manage))
          (setq found manage)))
    (if found (expand-file-name found))))

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
  (start-process "djangoserver" "*djangoserver*"
                 (django-manage)
                 "runserver_plus")
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
  (let ((url "http://localhost:8000"))
    (browse-url url)))

;; Testing
(defun django-test()
  "Run tests here"
  (interactive)
  (let ((func (django-get-func))
        (class (django-get-class))
        (app (django-get-app))
        (command nil))
    (if (string= "test" (substring func 0 4))
        (setq command (concat app "." class "." func)))
    (if command
        (start-process "djangotests" "*tests*"
                       (django-manage)
                       "test"
                       command)))
  (pop-to-buffer (get-buffer "*tests*")))

;; Keymaps

(defvar django-minor-mode-map
  (let ((map (make-keymap)))
    map))
(define-key django-minor-mode-map "\C-c\C-db" 'django-browser)
(define-key django-minor-mode-map "\C-c\C-dfd" 'django-fabric-deploy)
(define-key django-minor-mode-map "\C-c\C-dr" 'django-runserver)
(define-key django-minor-mode-map "\C-c\C-dt" 'django-test)

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
