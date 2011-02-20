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

;; Keymaps

(defvar django-minor-mode-map
  (let ((map (make-keymap)))
    map))
(define-key django-minor-mode-map "\C-c\C-db" 'django-browser)
(define-key django-minor-mode-map "\C-c\C-dr" 'django-runserver)

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
