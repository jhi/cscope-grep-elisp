;; cscope-grep.el -- M-x grep style interface to cscope(1)
;;
;; Jarkko Hietaniemi <jhi@iki.fi>
;;
;; Usage:
;;
;; (require 'cscope-grep)
;; (defun cscope-grep-key-bindings-hook ()
;;   (define-key c-mode-base-map "\C-c\C-c" 'cscope-grep-uses)
;;   (define-key c-mode-base-map "\C-c\C-d" 'cscope-grep-definitions)
;;   (define-key c-mode-base-map "\C-c\C-f" 'cscope-grep-callers)
;;   (define-key c-mode-base-map "\C-c\C-g" 'cscope-grep-callees)
;;   (define-key c-mode-base-map "\C-c\C-r" 'cscope-grep-reindex))
;; (add-hook 'c-mode-common-hook 'cscope-grep-key-bindings-hook)
;;
;; The cscope-grep-uses, cscope-grep-definitions, cscope-grep-callers,
;; cscope-grep-callees return their results in buffers called e.g.
;; "*cscope uses*", and as the results from M-x have lines of the form
;; "file:line: ...", which can be used to jump to the respective lines
;; in the source files.  By default they look for the symbol at the cursor,
;; but understand completion for all the known symbols.
;;
;; The cscope-grep-callers is for callers of the symbol.
;;
;; The cscope-grep-callees is for the calls made from this symbol.
;;
;; The cscope-grep-reindex can be used to explicitly generate the cscope
;; index files.  It is implicitly called by the above commands if none
;; exist yet.  At any given moment there can be only one index,
;; this limitation is inherited from the standard cscope.
;;
;; ---
;; Copyright (c) 2016 Jarkko Hietaniemi <jhi@iki.fi>
;; 
;; Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted, provided that the above copyright notice and this permission notice appear in all copies.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;; ---

(defvar cscope-grep-index nil
  "The filename of the cscope index.")
(defvar cscope-grep-index-directory nil
  "The directory name of the cscope index.")
(defvar cscope-grep-executable
  (executable-find "cscope")
  "The cscope executable being used.")
(defvar cscope-grep-index-opts
  "-Rubq"
  "The cscope indexing options.")
(defvar cscope-grep-symbols-map
  nil
  "The hashmap of all the known symbols.  Used for symbol completion.") 

(defun cscope-grep-buffer-cwd ()
  "Helper for finding out the directory of the current buffer."
  (file-name-directory (buffer-file-name)))

(defun cscope-grep-process-lines (type sym)
  "Helper for searching the cscope index file."
  (process-lines
   cscope-grep-executable
   (format "-f%s" cscope-grep-index) (format "-dqL%s" type) sym))

(defun cscope-grep-set-symbols-map ()
  "Recompute the hashmap cscope-grep-symbols-map which has all the known symbols as keys."
  (setq cscope-grep-symbols-map (make-hash-table :test 'equal))
  (mapcar
   (lambda (x)
     (if (string-match "^.+? \\(.+?\\) " x)
	 (puthash (match-string 1 x) t cscope-grep-symbols-map)))
   (cscope-grep-process-lines 2 ".*"))
  cscope-grep-symbols-map)

(defun cscope-grep-plurals (n s p)
  "Helper for prettyprinting.  n is the count, s the noun, p is the plural ending: either -suffix, or a full replacement."
  (format "%d %s"
          n
          (if (= n 1) s
            (if (string-match "^-\\(.+\\)" p)
                (concat s (match-string 1 p))
              p))))

(defun cscope-grep-reindex ()
  "Recompute the cscope index.  Also recomputes the cscope-grep-symbols-map as a side effect."
  (interactive)
  (if cscope-grep-executable
    (let* ((cwd (cscope-grep-buffer-cwd))
           (cmd (format "%s %s" cscope-grep-executable cscope-grep-index-opts))
           (cscope-dir
            (expand-file-name
             (read-directory-name (format "Run '%s' in: " cmd) cwd)))
           (default-directory cscope-dir)
           (cscope-index
            (concat cscope-dir "cscope.out")))
      (display-buffer "*Messages*")
      (message "Running %s in %s..." cmd cscope-dir)
      (if (shell-command cmd)
          (message "Generated %s" cscope-index)
        (message "Running %s failed"))
      (if (file-exists-p cscope-index)
          (progn
            (setq cscope-grep-index cscope-index)
            (setq cscope-grep-index-directory cscope-dir)
            (message "Finding all symbols...")
            (cscope-grep-set-symbols-map)
            (message "Found %s."
                     (cscope-grep-plurals
                      (hash-table-size cscope-grep-symbols-map)
                      "symbol" "-s")))
        (message "Failed to generate %s" cscope-index))
      )
    (error "Cannot find cscope executable in %s" (getenv "PATH"))
    ))

(defun cscope-grep-maybe-reindex ()
  "Recomputes the cscope index if it is undefined."
  (interactive)
  (if (not cscope-grep-index)
      (if (y-or-n-p (format "Undefined cscope index, reindex "))
          (cscope-grep-reindex))
    cscope-grep-index))

(defun cscope-grep-buffer (sym type desc)
  "Given a symbol, lookup type, and lookup description, run the cscope lookup, create a grep-style buffer for the results, and display the buffer."
  (let ((cscope-buf (get-buffer-create (format "*cscope %s*" desc))))
    (with-current-buffer cscope-buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (message "Searching %s..." sym)
      (let* ((matches (cscope-grep-process-lines type sym))
	     (nmatch (length matches)))
	(mapcar
	 (lambda (x)
	   (insert
	    (if (string-match "^\\(.+?\\) \\(.+?\\) \\(.+?\\) \\(.*\\)" x)
		(let ((filename (match-string 1 x))
		      (scope    (match-string 2 x))
		      (line     (match-string 3 x))
		      (rest     (match-string 4 x)))
		  (format "%s:%s: %s\t%s\n"
                          ;; TODO: it would be nice to do
                          ;; (file-relative-name ... orig-buf) for
                          ;; shorter nicer filenames, but the problem
                          ;; is that these filenames would be relative
                          ;; to the buffer at the moment of generating
                          ;; the cscope buffer, not when the match
                          ;; lines are accessed.
                          (if (file-name-absolute-p filename)
                              filename
                            (concat cscope-grep-index-directory filename))
			  line
			  scope
			  rest)))))
	 matches)
	(grep-mode)
	(goto-char (point-min))
	(display-buffer cscope-buf)
	(message "Found %s" (cscope-grep-plurals nmatch "match" "-es"))
	))))

(defun cscope-grep-aux (type desc)
  "Helper for cscope-grep-uses and its like."
  (if (cscope-grep-maybe-reindex)
      (cscope-grep-buffer
       (completing-read (format "cscope-grep %s: " desc)
			cscope-grep-symbols-map
			nil
			nil
			(thing-at-point 'symbol))
       type desc)
    (message "No cscope index.")))

(defun cscope-grep-uses ()
  "Look for uses (definitions or calls) of a symbol."
  (interactive)
  (cscope-grep-aux 0 "uses"))
(defun cscope-grep-definitions ()
  "Look for definitions of a symbol."
  (interactive)
  (cscope-grep-aux 1 "definitions"))
(defun cscope-grep-callees ()
  "Look for calls made from the implementation of a symbol."
  (interactive)
  (cscope-grep-aux 2 "callees"))
(defun cscope-grep-callers ()
  "Look for callers of a symbol."
  (interactive)
  (cscope-grep-aux 3 "callers"))

(provide 'cscope-grep)
