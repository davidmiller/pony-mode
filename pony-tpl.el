;;
;; Pony-tpl-minor-mode
;;
;; Commentary:
;;
;; This minor mode provides syntax highlighting and some useful default
;; shortcuts for editing django template (html) files.
;;

;; pony-tpl-minor-mode begins

(defgroup pony-tpl nil
  "Djangification for Templates in Emacs"
  :group 'pony
  :prefix "pony-tpl-")

(defcustom pony-tpl-block-re
  "\{% ?block ?[a-zA-Z]?+ ?%\}"
  "Regexp to match blocks in Django templates"
  :group 'pony-tpl
  :type 'string)

(defcustom pony-tpl-inent-start
  "\{% ?block ?[a-zA-Z]?+ ?%\}"
  "Regexp to match the opening tag of a pair that should mark indentation in a Django template"
  :group 'pony-tpl
  :type 'string)

(defcustom pony-tpl-inent-end
  "\{% ?endblock ?[a-zA-Z]?+ ?%\}"
  "Regexp to match the end tag of a pair that should mark indentation in a Django template"
  :group 'pony-tpl
  :type 'string)

;;
;; Indentation of Django tags
;;
;; Commentary:
;;
;; By default sgml-mode's `sgml-indent-line' indents html files by checking the lexical
;; context of `point' and if this is deemed to be text, uses a somewhat inflexible
;; (while (looking-at "</") BODY) to determine the correct indentation level. There is no
;; `sane' way to override this regexp, so we wrap `sgml-indent-line' here.
;;

(defun pony-tpl-indent-level nil
  "Calculate the number of indentation levels that Django Template syntax
dictates should be added to the HTML indentation level at `point`.

The heuristic here is fairly simple - we only interpret template tags with both
opening and closing tags as requiring indentation, and then subtract the number of 
closing tags from opening tags before point.

The precise nature of what is interpreted as an indent-worthy tag can be overidden
with the values of `pony-tpl-indent-start' and `pony-tpl-indent-end'."
  (let ((pony-indent (- (count-matches pony-tpl-indent-start 0 (point)) 
                        (count-matches pony-tpl-indent-end 0 (point))))
        (sgml-indent (sgml-calculate-indent))))
  (+ sgml-indent (* sgml-basic-offset pony-indent)))

(defun pony-indent nil
  "The buffer-local indent-line function for pony-tpl buffers."
  (indent-line-to (pony-tpl-indent-level)))

;; (defadvise sgml-iqndent-line after BODY)

(defvar pony-tpl-mode-hook nil)

(defconst pony-tpl-font-lock-keywords
  (append
   sgml-font-lock-keywords
   (list
    '("{%.*\\(\\bor\\b\\).*%}" . (1 font-lock-builtin-face))
    ;'("{% ?comment ?%}\\(\n?.*?\\)+?{% ?endcomment ?%}" . font-lock-comment-face)
    '("{#.*#}" . font-lock-comment-face)
    '("{% ?\\(\\(end\\)?\\(extends\\|for\\|cache\\|cycle\\|filter\\|firstof\\|debug\\|if\\(changed\\|equal\\|notequal\\|\\)\\|include\\|load\\|now\\|regroup\\|spaceless\\|ssi\\|templatetag\\|widthratio\\|block\\|trans\\)\\) ?.*? ?%}" . 1)
    '("{{ ?\\(.*?\\) ?}}" . (1 font-lock-variable-name-face))
    '("{%\\|\\%}\\|{{\\|}}" . font-lock-builtin-face)
    ))
  "Highlighting for pony-tpl-mode")

(define-minor-mode pony-tpl-minor-mode
  "Pony-templatin-riffic"
  :initial nil
  :lighter " PonyTpl"
  :keymap pony-minor-mode-map)

(defun pony-tpl-mode()
  "Minor mode for editing pony templates"
  (interactive)
  (pony-tpl-minor-mode t)
  (run-hooks 'pony-tpl-mode-hook)
  (set (make-local-variable 'font-lock-defaults)
       '(pony-tpl-font-lock-keywords))
   (pony-load-snippets))

;; pony-tpl-minor-mode ends