;;; ideone.el --- IDEone for emacs!
;;
;; Copyright (C) 2012 Brian Fransioli
;;
;; Author: Brian Fransioli <assem@terranpro.org>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Emacs interface for posting to ideone.com via their SOAP API.
;; Includes support for submissions in any of their choice of
;; supported languages.  Can download from another user's ideone post.
;; Maintains a list of recently submitted posts, to review results,
;; redownload code, or share links easily.
;; 
;; To use, place in your .emacs or similar:
;; (load-library "~/path/to/ideone.el")
;; (setq ideone-user "ideone_id")
;; (setq ideone-pass "ideone_api_pass")
;; (ideone-init)

;;; Code:

(load-library "soap-client")
(load-library "soap-inspect")
(require 'ido)
(require 'assoc)
(require 'cl)

;;(setq ideone-wsdl-url "http://ideone.com/api/1/service.wsdl")
;;(setq ideone-wsdl (soap-load-wsdl-from-url ideone-wsdl-url))
(setq ideone-wsdl-file "ideone.wsdl")
(setq ideone-wsdl-file-full (if load-in-progress
				(format "%s/%s" 
					(file-name-directory load-file-name)
					ideone-wsdl-file)
			      (format "%s/%s"
				      (file-name-directory buffer-file-name)
				      ideone-wsdl-file)))

(setq ideone-wsdl (soap-load-wsdl ideone-wsdl-file-full))

(defvar ideone-user "test"
  "IDEone User ID")
(defvar ideone-pass "test"
  "IDEone API Password")

(defvar ideone-languages-alist nil
  "Assoc List of Supported Languages Returned from IDEOne API")

(defvar ideone-submit-lang nil
  "Buffer local variable to force submission using a certain type.
Should set to a pattern like C++, C99, Scheme, Python, etc.")

(defvar ideone-recent-submissions ()
  "Recent submission ids with most recent at the top/beginning.")

(defvar ideone-lang-mode-alist
  '(("C99" . c-mode)
    ("C99 strict" . c-mode)
    ("C" . c-mode)
    ("C++0x" . c++-mode)
    ("C++" . c++-mode)
    ("python" . python-mode)
    ("lisp" . lisp-mode)
    ("text" . text-mode)
    ("Assembler" . asm-mode)
    ("AWK" . awk-mode)
    ("Bash" . sh-mode)
    ("Common Lisp" . common-lisp-mode)
    ("Java" . java-mode)
    ("JavaScript" . javascript-mode)
    ("Pascal" . pascal-mode)
    ("Perl" . perl-mode)
    ("Objective-C" . objc-mode)
    ("Prolog" . prolog.mode)
    ("R" . r-mode)
    ("Ruby" . ruby-mode)
    ("Scheme" . scheme-mode)
    ("SQL" . sql-mode)
    ("Tcl" . tcl-mode)
    ("Whitespace" . text-mode))

  "Mapping of language identifiers to their emacs mode.  Note,
  the order *IS* important here!  It's hack-ish to prefer C++0x
  over C++ and C99 over C.  This solves the reserve lookup
  problem so we don't have to search the ideone language strings
  for \"C\" and pray we get the C, gcc compiler tag (C99 is easy
  to find).")

(defvar ideone-header-end "$$$IDEONE_HEADER_END$$$"

  "String describing a tag we stick in the paste to easily find
  the end of the IDEone header; this way, we can modify a paste
  and resubmit without submitting the previous headers.")

;; TODO: these are unused currently...
(setq ideone-status-alist '((-1 . "waiting")
			    ( 0 . "done")
			    ( 1 . "compiling")
			    ( 3 . "running")))

(setq ideone-result-alist '((0  . "not running")
			    (11 . "compile error")
			    (12 . "runtime error")
			    (13 . "time limit exceeded")
			    (15 . "success")
			    (17 . "memory limit exceeded")
			    (19 . "illegal system call")
			    (20 . "internal error")))

(setq ideone-error-alist '((ok . "OK")
			   (auth_error . "AUTH_ERROR")
			   (paste_not_found . "PASTE_NOT_FOUND")
			   (wrong_lang_id . "WRONG_LAND_ID")
			   (access_denied . "ACCESS_DENIED")
			   (cannot_submit_this_month_anymore . "LIMIT")))

(defun ideone-enable-debug ()
  "Turn on debugging for Emacs and SOAP for backtraces if
problems should occur."
  (setq debug-on-error t)
  (setq soap-debug t))

(defun convert-pair (pair) `(,(cdr (third pair)) . ,(cdr (second
							  pair))))

(defun convert-soap (data key-field value-field) 
  (loop for element in data
	collect (cons (cdr (assoc key-field element))
		      (cdr (assoc value-field element)))))

(defun ideone-invoke (cmd &rest args)
  "Wrapper function around `soap-invoke' which sends an IDEone
command."
  (car (apply 'soap-invoke 
	      ideone-wsdl 
	      "Ideone_Service_v1Port" 
	      cmd 
	      ideone-user 
	      ideone-pass
	      args)))

(defun ideone-check-error (response)
  "Intended to check for errors from command responses; TODO.
Not used yet beyond the languages command."
  (let* ((error-fields response)
	 (name (cdr (first (car error-fields))))
	 (status (cdr (second (car error-fields))))
	(error-response))
    `(,name . ,status)))

(defun ideone-conv-languages-alist (response)
  "Scan a SOAP response until we find the languages section.
HACKISH.  TODO fix later."
  (let* ((language-fields))
    (while (and (not (rassoc "languages" (car (car response))))
		(not (eq (length response) 0)))
      (pop response))

    (setq languages-fields (car (car response)))
))

(defun ideone-parse-simple (response)
  "Parse a simple response (non-recursive) into a simple alist of
the form (param . value).  For example (\"response\" . \"OK\")."

  (mapcar '(lambda (pair)
	     `(,(cdr (first pair)) . ,(cdr (second pair)))) 
	  response))

(defun ideone-parse-languages (response)
  "Parse a complex, languages response (recursive) into a simple
alist.  Similar to `ideone-parse-simple'."
  (let ((languages (cdr (second (car (cdr response))))))
    (mapcar 'convert-pair languages)))

(defun ideone-value-from-presponse (presponse key)
  "Grab the value for KEY of parsed response, PRESPONSE."
  (let ((value)) 
    (loop for pair in presponse
	  do (if (string-match key (car pair))
		 (setq value (cdr pair)))
	  finally return value))
)

(defun ideone-trim-lang-response (lresponse)
  "Trim the language response to include only the first word, and 
exclude anything in parentheses, compilers, etc."

  (let ((regexp "\\(\\w+[+-]*\\w*\\)"))
    (if (string-match regexp lresponse)
	(match-string 1 lresponse))))

(defun ideone-show-output-source (presponse)
  "Show output of a parsed response, PRESPONSE, in a temporary
buffer, including the original source code, in a form ready for
the user to modify, with the remainder of the response
fields (errors, output, time, etc.) in a commented region."

  (let* ((buffer "*IDEone*")
	 (lang (ideone-trim-lang-response 
		(ideone-value-from-presponse presponse "langName")))
	 (lang-mode (or (aget ideone-lang-mode-alist lang)
			nil))
	 (time (ideone-value-from-presponse presponse "time"))
	 (source (ideone-value-from-presponse presponse "source"))
	 (input (ideone-value-from-presponse presponse "input"))
	 (output (ideone-value-from-presponse presponse "output"))
	 (cmpinfo (ideone-value-from-presponse presponse "cmpinfo"))) 
    (with-output-to-temp-buffer buffer
      (with-current-buffer buffer
	(if (null lang-mode)
	    (text-mode)
	  (apply lang-mode nil))

	(let ((startp (point)))
	  (insert (format "Time: %s\n\nLang: %s\n\nCompiler Output:\n%s\n" 
			  time lang cmpinfo))
	  (insert (format "Input:\n%s\n\nOutput:\n%s\n\n" input output))
	  (insert (format "%s\n\n" ideone-header-end))
	  (setq comment-style 'indent)
	  (comment-region startp (point) 2)
	  (insert source)
	  (goto-char (point-min))))))  
  t)

(defun ideone-show-output (presponse)
  "Show output of a parsed response, PRESPONSE, in a temporary buffer.
PRESPONSE should have been parsed with something like `ideone-parse-simple'."

  (let ((buffer "*IDEone*")) 
    (with-output-to-temp-buffer buffer
      (with-current-buffer buffer
       (mapcar '(lambda (pair)
		  (let ((label)
			(value)
			(out)) 
		    (setq label (car pair))
		    (setq value (cdr pair))
		    (if (and (stringp value) 
			     (string-match "\n" value))
			(setq out (format "%s: \n%s\n" label value))
		      (setq out (format "%s: %s\n" label value)))
		    (insert out))) 
	       presponse))))
  t)

(defun ideone-get-languages ()
  "Invoke the getLanguages command and parse the response,
sending it to the variable, `ideone-languages-alist'."

  (let ((languages-response (ideone-invoke "getLanguages")))
    (ideone-check-error languages-response)
    (setq ideone-languages-alist (ideone-parse-languages languages-response))))

;;(ideone-parse-simple (ideone-submission-status "Td2NC"))
;;(ideone-show-output ideone-result)

(defun ideone-get-lang-code-from-string (s)
  "Match a string in `ideone-languages-alist' to find the numeric
code used for submission."

  (assoc-default (regexp-quote s) ideone-languages-alist
		 '(lambda (elem key)
		    (cond ((not (stringp elem)) nil)
			  ((string-match key elem) t)))))

(defun ideone-guess-language ()
  "Using the major mode, make a guess at the submission language,
and thereby the numeric code used for submission."

  (let ((lang)
	(code))
    (setq lang (case major-mode
		 ((c-mode "C99"))
		 (c++-mode "C++0x")
		 (t "Text")))
    (setq code (ideone-get-lang-code-from-string lang)))
  )

(defun ideone-create-submission ()
  "Create a submission using the current buffer or the active
region of the current buffer.  The submission language, if not
set by `ideone-set-submission-lang' will be guessed using the
major-mode.  Compilation is enabled by default.  TODO: Work with
input (stdin) to the submission."

  (interactive)
  (let* ((end (if (use-region-p)
		  (region-end)
		(point-max)))
	 (beg (if (use-region-p)
		  (region-beginning)
		(save-excursion
		  (goto-char (point-min))
		  (let ((begh (search-forward-regexp 
			       (regexp-quote ideone-header-end) end t)))
		    (if (null begh)
			(point-min)
		      (move-end-of-line nil)
		      (forward-char 1)
		      (if (search-forward-regexp "\\S-" end t)
			  (backward-char 1))
		      (point))))))
	 (code (buffer-substring-no-properties beg end))
	 (language (cond ((not (null ideone-submit-lang)) 
			  (ideone-get-lang-code-from-string ideone-submit-lang))
			 (t (ideone-guess-language))))
	 (result (ideone-invoke "createSubmission" 
				code
				language
				""
				t
				nil))
	 (link (cdr (assoc "link" (ideone-parse-simple result))))
	 (url (format "http://ideone.com/%s" link)))
    (ideone-show-output result)

    (while (not (= (ideone-submission-status link) 0))
      (message (format "Code:%s" code))
      (message "Waiting for compilation...")
      )
    (message "IDEone Submission at Link: %s" url)
    (ideone-submission-details link)
    (with-temp-buffer
      (insert url)
      (kill-ring-save (point-min) (point-max)))
    link))

(defun ideone-submission-status (id) 
  "Grab the submission status for snippet ID and return the
numeric code."

  (let ((result (ideone-invoke "getSubmissionStatus" 
					   id)))
    (ideone-show-output result)
    (cdr (assoc "status" (ideone-parse-simple result)))))

(defun ideone-submission-details (id)
  "Grab the submission details for snippet ID, parse it, and
display the details in a separate buffer."

  (let ((result (ideone-invoke "getSubmissionDetails" 
					   id
					   t
					   t
					   t
					   t
					   t)))
    (setq ideone-recent-submissions 
	  (cons (list id) ideone-recent-submissions))
    (ideone-show-output-source (ideone-parse-simple result))))

;; found `aget' instead
;; (defun aval (alist key)
;;   "Returns the value of `key` in `alist`."
;;   (cdr (assoc key alist)))

(defun ideone-id-from-url (url)
  "Grab the ID part of an IDEone url, URL."

  (let ((id-regexp "ideone.com/\\([\w\d0-9A-Za-z]*\\)"))
    (string-match id-regexp url)
    (match-string 1 url)))

;;(ideone-id-from-url "http://ideone.com/45AzL")

(defun ideone-get-submission ()
  "Grab a submission either by an IDEone URL at point, or by
using a completing read from the user.  The completing read uses
the recent submissions lists, and can be of the form
http://ideone.com/ID or ID."

  (interactive)
  (let ((url (thing-at-point-url-at-point))
	(link))
    (if (null url)
	(setq url (ido-completing-read "ideone url or id: "
				       (mapcar '(lambda (c) (car c))
					       ideone-recent-submissions))))
    (with-temp-buffer
      (insert url)
      (if (null (thing-at-point-url-at-point))
	  (setq link url)
	(setq link (ideone-id-from-url url))))
    (ideone-submission-details link)))

(defun ideone-init ()
  "Invoke the test function to make sure our credentials are
accepted, and then grab and parse the languages into
`ideone-languages-alist'."

  (if (not (string=  (aget (ideone-parse-simple (ideone-invoke "testFunction"))
			   "error")
		     "OK"))
      (error "Auth Error"))
  (ideone-get-languages) t)

(defun ideone-set-submission-lang ()
  "Manually set (force) a submission language.  This is useful
for situations where guessing does not choose the correct
submission type; this could include selecting the correct
compiler (C++: standard vs c++11 or ASM: nasm vs gcc).  TODO:
Make this buffer local later!"

  (interactive)
  (setq ideone-submit-lang 
	(ido-completing-read "Submission Language: " 
			     (mapcar '(lambda (p) (car p)) 
				     ideone-languages-alist))))

;; (defun ideone-hook ()
;;   (local-set-key (kbd "C-c I s") 'ideone-create-submission)
;;   (local-set-key (kbd "C-c I l") 'ideone-set-submission-lang)
;;   (local-set-key (kbd "C-c I g") 'ideone-get-submission)
;;   ;; reserved for future
;;   ;;(local-set-key (kbd "C-c I u") 'ideone-update-submission)
;;   )

;; (add-hook 'find-file-hook 'ideone-hook)
;; (add-hook 'c-mode-common-hook 'ideone-hook)

;; Hacked global keymap.  TODO: finesse later
(global-set-key (kbd "C-c I s") 'ideone-create-submission)
(global-set-key (kbd "C-c I l") 'ideone-set-submission-lang)
(global-set-key (kbd "C-c I g") 'ideone-get-submission)

(provide 'ideone)

;;; ideone.el ends here
