
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

;;;
;;; Unit tests begin
;;;

;;;
;;; pony-mode.el
;;;

(ert-deftest pony-test-chomp ()
  "Should kill leading and tailing whitespace"
  (should (equal "Hello Beautiful World" (pony-chomp " Hello Beautiful World "))))

(ert-deftest pony-find-file ()
  "find some py files"
  (let ((path (path.join *ponytestbase* "data/ponytester"))
        (pattern "manag"))
    (should (equal (list (path.join *ponytestbase* "data/ponytester/manage.py"))
                   (pony-find-file path pattern)))))

;;; pony-locate

(ert-deftest pony-read-file ()
  "File contents to string."
  (should (equal "Hello Beautiful World!"
                 (pony-read-file (path.join *ponytestbase* "data/HELLO.txt")))))


;;;
;;; pony-tpl.el
;;;
(ert-deftest pony-calculate-indent-multitags ()
  "Calculate indents in our test file - regression re #51"
  (save-excursion
    (let ((tpl (find-file (path.join *ponytestbase* "data/ponytester/templates/wholefile.html"))))
      (switch-to-buffer tpl)
      (forward-line 2)
      (should (equal 3 (line-number-at-pos)))
      (should (equal 0 (pony-calculate-indent))))))
