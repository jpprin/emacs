;; Emacs Lisp utilities

;************************  LISP MACROS  ************************

;; Apr 16 2017
(defmacro every-p (lst)
  "Takes a function and returns T if every element is true."
  (cons 'and (eval lst)))

;; Dec 13 2017
(defmacro mac (expr)
  "A macro shorthand to pretty-print macro expansions"
  `(pp-macroexpand-expression ',expr))


;***********************  LISP FUNCTIONS  ***********************

(defun mapcar* (f &rest xs) 
  "MAPCAR for multiple sequences"
  (if (not (memq nil xs))
      (cons (apply f (mapcar 'car xs))
	    (apply 'mapcar* f (mapcar 'cdr xs)))))

(defun range (start &optional end step) 
  "A range function similar to Python's."
  (unless end
    (setq end start
	  start 0))
  (number-sequence start (1- end) step))

  (defsubst square (x)
  "Return the square of X."
  (* x x))

(defun nshuffle (sequence)
  "An implementation of the Knuth shuffle."
  (cl-loop for i from (length sequence) downto 2
	   do (cl-rotatef (elt sequence (random i))
			  (elt sequence (1- i))))
  sequence)

;***********************  FILE FUNCTIONS  ***********************

(defun load-data (file) 
  "Return the contents of FILE as a Lisp object."
  (car (read-from-string
	(with-temp-buffer
	  (insert-file-contents file)
	  (buffer-string)))))


;**********************  BUFFER FUNCTIONS  **********************

(defun writing-font () 
  "Preferred frame font for prose writing"
  (interactive)
  (set-frame-font "Calibri 11"))

(defun insert-date ()
  "Insert a dat in the format MMM DD YYYY."
  (interactive)
  (insert (format-time-string "%b %d %Y" (current-time))))

;; key combination for `insert-date'
(global-set-key (kbd "C-c C-d") 'insert-date)


(defvar *rtf-chars* 
  '(("’" . "'")
    ("‘" . "'")
    ("“" . "\"")
    ("”" . "\"")
    ("–" . "-")
    ("…" . "..."))
  "A list of non-ASCII characters that can't be saved in a text file.")

(defun clean-text ()
  "Replace all the rich text quotes with ASCII quotes"
  (interactive)
  (save-excursion
    (mapc
     #'(lambda (pair) (progn (goto-char (point-min))
			(replace-string (car pair) (cdr pair))))
     *rtf-chars*)))

(defun capitalize-all (start end)
  "Capitalize all of the lower-case characters in the region"
  (interactive (list (region-beginning) (region-end)))
  (if (use-region-p)
      (let* ((old-str (buffer-substring start end))
         (new-str (map 'string (lambda (n) (if (and (>= n 97)
        (<= n 122))
                          (- n 32)
                        n)) old-str)))
    (progn
      (delete-region start end)
      (goto-char start)
      (insert new-str)))))

(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single logical
line.  Useful when switching between fundamental mode and visual line mode."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;; key binding for `unfill-region'
(define-key global-map "\C-\M-Q" 'unfill-region)

(defun regexp-matches (regexp string)
  "Return a list of all regexp matches found in a string"
  (save-match-data
    (let ((pos 0)
	  matches)
      (while (string-match regexp string pos)
	(push (match-string 0 string) matches)
	(setq pos (match-end 0)))
      matches)))

(defun number-grouping (number &optional separator)
  "Add commas to NUMBER and return it as a string.
    Optional SEPARATOR is the string to use to separate groups.
    It defaults to a comma."
  (let ((num (number-to-string number))
	(op (or separator ",")))
    (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
      (setq num (concat 
		 (match-string 1 num) op
		 (match-string 2 num))))
    num))

(defun insert-comment-header (header)
  "Insert HEADER surrounded by asterisks as the header for a section of code."
  (interactive "sEnter the section header: ")
  (let* ((comment-line-length 64)     ; nice looking number
	 (spaces (make-string 2 32))  ; two spaces on each side

	 ; capitalize the text
	 (upcase-header (upcase header))
	 
	 ; make space for header string and two spaces on each side
	 (header-length (+ (length header) (* 2 (length spaces))))
	 
  	 ; get enough stars to fill it out to 75 spaces
	 (stars (make-string (/ (- comment-line-length header-length) 2) ?*))

	 ; insert a semi-colon to start
	 (comment-start (make-string 1 59))
	 
	 ; put them all together
	 (comment-string  (concat comment-start stars spaces upcase-header spaces stars)))

    (insert comment-string)))


;*********************  BROWSER FUNCTIONS  *********************

(defun google () 
  "Opens a brower and sends a Google search request."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if mark-active
	(buffer-substring (region-beginning) (region-end))
      (read-string "Google: ")))))

