
;;; Setup of test utils/state
(defvar *ponytestbase* (file-name-directory (or buffer-file-name load-file-name)))

(add-to-list 'load-path (expand-file-name (concat *ponytestbase* "resources")))

(require 'ert)
(require 'pony-mode)

(defun path.join (base &rest paths)
  "Translation of Python's os.path.join. Take path elements and
join them intelligently.

If any element is an abolute path, discard
all previous elements. Otherwise, concatenate base and all paths
elements joined by \."
  (let ((path base))
    (dolist (item paths)
      (if (string= "/" (substring item 0 1))
          (setq path item)
        (if (string= "/" (substring path -1))
            (setq path (concat path item))
          (setq path (concat path "/" item)))))
    path))

(defmacro pony-deftest-excursion (name path docstring body)
  "Write an ERT test called NAME where we move to the file below *PONYTESTBASE*
indicated by PATH before executing BODY"
  (let ((target (path.join *ponytestbase* path)))
    `(ert-deftest ,name ()
      ,docstring
      (save-excursion
        (find-file ,target)
        ,body))))

;;;
;;; Unit tests begin
;;;

;;;
;;; pony-mode.el
;;;

(ert-deftest pony-chomp ()
  "Should kill leading and tailing whitespace"
  (should (equal "Hello Beautiful World" (pony-chomp " Hello Beautiful World "))))

(ert-deftest pony-find-file ()
  "find some py files"
  (let ((path (path.join *ponytestbase* "data/ponytester"))
        (pattern "manag"))
    (should (equal (list (path.join *ponytestbase* "data/ponytester/manage.py"))
                   (pony-find-file path pattern)))))

(ert-deftest pony-find-file-p ()
  "find some py files"
  (let ((path (path.join *ponytestbase* "data/ponytester"))
        (pattern "manag"))
    (should (equal t (pony-find-file-p path pattern)))))

(pony-deftest-excursion pony-locate "data/ponytester/settings.py"
  "Let's find the settings file"
  (should (equal (path.join *ponytestbase* "data/ponytester/.dir-locals.el")
                 (pony-locate ".dir-locals.el"))))

(ert-deftest pony-read-file ()
  "File contents to string."
  (should (equal "Hello Beautiful World!"
                 (pony-read-file (path.join *ponytestbase* "data/HELLO.txt")))))

(ert-deftest pony-pop ()
  "We should be able to pop to a buffer and also return it."
  (save-excursion
    (should (equal (pony-pop "foobuff") (get-buffer "foobuff")))))

(pony-deftest-excursion pony-configfile-p  "data/ponytester/settings.py"
 "Proj has a configfile"
 (should (equal t (pony-configfile-p))))

(pony-deftest-excursion pony-rc "data/ponytester/settings.py"
  "Configfile should be nil"
  (should (equal nil (pony-rc))))

(pony-deftest-excursion pony-proj-newstructure-p "data/ponytester/settings.py"
  "Say no to oldstructure"
  (should (equal nil (pony-project-newstructure-p))))

(ert-deftest pony-active-python ()
  "Get our active Python interpreter"
  (let ((settingsfile (path.join *ponytestbase* "data/ponytester/settings.py")))
    (save-excursion
      (should (equal (executable-find "python") (pony-active-python))))))

(ert-deftest pony-command-exists-t ()
  "Should be a valid command"
  (let ((settingsfile (path.join *ponytestbase* "data/ponytester/settings.py")))
    (save-excursion
      (find-file settingsfile)
      (should (equal t (pony-command-exists-p "runserver"))))))

(ert-deftest pony-command-exists-f ()
  "Should be a valid command"
  (let ((settingsfile (path.join *ponytestbase* "data/ponytester/settings.py")))
    (save-excursion
      (find-file settingsfile)
      (should (equal nil (pony-command-exists-p "like_this_is_a_command"))))))

(ert-deftest pony-get-settings-file-basename ()
  "Should return the default basename"
  (let ((settingsfile (path.join *ponytestbase* "data/ponytester/settings.py")))
    (save-excursion
      (find-file settingsfile)
      (should (equal "settings" (pony-get-settings-file-basename))))))

(ert-deftest pony-get-settings-module ()
  "Should return the absolute path to the settings module"
  (let ((settingsfile (path.join *ponytestbase* "data/ponytester/settings.py")))
    (save-excursion
      (find-file settingsfile)
      (should (equal settingsfile (pony-get-settings-module))))))

(ert-deftest pony-setting-p-t ()
  "This setting exists"
  (let ((settingsfile (path.join *ponytestbase* "data/ponytester/settings.py")))
    (save-excursion
      (find-file settingsfile)
      (should (equal t (pony-setting-p "DEBUG"))))))

;;;
;;; pony-tpl.el
;;;
(ert-deftest pony-calculate-indent-multitags ()
  "Calculate indents in our test file - regression re #51"
  (save-excursion
    (let ((tpl (find-file (path.join *ponytestbase* "data/ponytester/templates/wholefile.html"))))
      (switch-to-buffer tpl)
      (goto-char (point-min))
      (forward-line 2)
      (should (equal 3 (line-number-at-pos)))
      (should (equal 0 (pony-calculate-indent))))))
