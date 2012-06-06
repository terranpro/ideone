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

;;; Code:

(load-library "soap-client")
(load-library "soap-inspect")
(require 'ido)

;;(setq ideone-wsdl-url "http://ideone.com/api/1/service.wsdl")
;;(setq ideone-wsdl (soap-load-wsdl-from-url ideone-wsdl-url))

(setq ideone-wsdl (soap-load-wsdl "./ideone.wsdl"))

(defvar ideone-user "test"
  "IDEone User ID")
(defvar ideone-pass "test"
  "IDEone API Password")

(defvar ideone-languages-alist nil
  "Assoc List of Supported Languages Returned from IDEOne API")

(defvar-local ideone-submit-lang nil
  "Buffer local variable to force submission using a certain type.
Should set to a pattern like C++, C99, Scheme, Python, etc.")

(defvar ideone-recent-submissions ()
  "Recent submission ids with most recent at the top/beginning.")

(defvar ideone-lang-mode-alist
  '(("C" . c-mode)
    ("C99" . c-mode)
    ("C99 strict" . c-mode)
    ("C++" . c++-mode)
    ("C++0x" . c++-mode)
    ("python" . python-mode)
    ("lisp" . lisp-mode)
    ("text" . text-mode))
  "Mapping of identifiers to their emacs mode.")

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
  (setq soap-debug t))
;;(ideone-enable-debug)

;; (setq ideone-supported-languages 
;;       (soap-invoke ideone-wsdl "Ideone_Service_v1Port" "getLanguages" 
;; 		   "terranpro" "capp1234"))



;; (setq ideone-languages (cdr (car (cdr (car ideone-supported-languages)))))
;; (rassoc "" ideone-languages)
;; (setq ideone-lang-rdy (car ideone-languages))
;; (pop ideone-lang-rdy)
;; (mapcar 'convert-pair (cdr (car ideone-languages)))
;; (setq ideone-lang-alist (mapcar 'convert-pair ideone-lang-rdy))

(defun convert-pair (pair) `(,(cdr (third pair)) . ,(cdr (second
							  pair))))

(defun convert-soap (data key-field value-field) 
  (loop for element in data
	collect (cons (cdr (assoc key-field element))
		      (cdr (assoc value-field element)))))

;;(convert-soap (car ideone-supported-languages) 'key 'value)


(defun ideone-invoke (cmd &rest args)
  (car (apply 'soap-invoke 
	      ideone-wsdl 
	      "Ideone_Service_v1Port" 
	      cmd 
	      ideone-user 
	      ideone-pass
	      args)))

(defun ideone-check-error (response)
  (let* ((error-fields response)
	 (name (cdr (first (car error-fields))))
	 (status (cdr (second (car error-fields))))
	(error-response))
    `(,name . ,status)))

(defun ideone-conv-languages-alist (response)
  (let* ((language-fields))
    (while (and (not (rassoc "languages" (car (car response))))
		(not (eq (length response) 0)))
      (pop response))

    (setq languages-fields (car (car response)))
))

;;(length (car ideone-supported-languages))
;;(ideone-conv-languages-alist ideone-supported-languages)

;; (setq ideone-temp ideone-supported-languages)
;; (pop (car ideone-temp))
;; (cdr (second (car (car ideone-supported-languages))))

;; (rassoc "error"  (car (car ideone-supported-languages)))

;; (cdr (rassoc (cdr (ideone-check-error ideone-supported-languages)) 
;; 	 ideone-error-alist))


;;(ideone-check-error (list (ideone-invoke "getLanguages")))
;;(ideone-get-languages)
(defun ideone-parse-simple (response)
  (mapcar '(lambda (pair)
	     `(,(cdr (first pair)) . ,(cdr (second pair)))) 
	  response))

(defun ideone-parse-languages (response)
  (let ((languages (cdr (second (car (cdr response))))))
    (mapcar 'convert-pair languages)))

;; Scratchpad testing for parsing this !@#$ing languages recursive alist
;;(cdr (second (car (cdr (ideone-invoke "getLanguages")))))
;;(mapcar 'convert-pair (cdr (car (cdr (second (car ideone-supported-languages))))))
;;(ideone-parse-languages (car ideone-supported-languages))

(defun ideone-value-from-presponse (presponse key)
  (let ((value)) 
    (loop for pair in presponse
	  do (if (string-match key (car pair))
		 (setq value (cdr pair)))
	  finally return value))
)

(defun ideone-show-output-source (presponse)
  "Show output of a parsed response, PRESPONSE, in a temporary
buffer, including the original source code, in a form ready for
the user to modify, with the remainder of the response
fields (errors, output, time, etc.) in a commented region."

  (let* ((buffer "*IDEone*")
	 (lang (ideone-value-from-presponse presponse "langName"))
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
  (let ((languages-response (ideone-invoke "getLanguages")))
    (ideone-check-error languages-response)
    (setq ideone-languages-alist (ideone-parse-languages languages-response))))

;;(ideone-parse-simple (ideone-submission-status "Td2NC"))
;;(ideone-show-output ideone-result)

(defun ideone-get-lang-code-from-string (s)
  (assoc-default (regexp-quote s) ideone-languages-alist
		 '(lambda (elem key)
		    (cond ((not (stringp elem)) nil)
			  ((string-match key elem) t)))))

(defun ideone-guess-language ()
  (let ((lang)
	(code))
    (setq lang (case major-mode
		 ((c-mode "C99"))
		 (c++-mode "C++0x")
		 (t "Text")))
    (setq code (ideone-get-lang-code-from-string lang)))
  )

(defun ideone-create-submission ()
  (interactive)
  (let* ((beg (if (use-region-p)
		  (region-beginning)
		(point-min)))
	 (end (if (use-region-p)
		  (region-end)
		(point-max)))
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
      (message "Waiting for compilation...")
      )
    (message "IDEone Submission at Link: %s" url)
    (ideone-submission-details link)
    (with-temp-buffer
      (insert url)
      (kill-ring-save (point-min) (point-max)))
    link))

;; (cond ( (null ideone-submit-lang) "Hi")
;;       (t "Bye"))

;;(setq ideone-result '(("error" . "OK") ("link" . "Td2NC")))

;;(cdr (assoc "status" (ideone-parse-simple (ideone-submission-status "Td2NC"))))

(defun ideone-submission-status (id) 
  (let ((result (ideone-invoke "getSubmissionStatus" 
					   id)))
    (ideone-show-output result)
    (cdr (assoc "status" (ideone-parse-simple result)))))

(defun ideone-submission-details (id)
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
  (let ((id-regexp "ideone.com/\\([\w\d0-9A-Za-z]*\\)"))
    (string-match id-regexp url)
    (match-string 1 url)))

;;(ideone-id-from-url "http://ideone.com/45AzL")

(defun ideone-get-submission ()
  (interactive)
  (let ((url (thing-at-point-url-at-point)))
    (if (null url)
	(setq url (ido-completing-read "ideone url or id: "
				       (mapcar '(lambda (c) (car c))
					       ideone-recent-submissions))))
    (ideone-submission-details (cond ((url-p url) (ideone-id-from-url url))
				     (t url)))))
(defun ideone-init ()
  (if (not (string=  (aget (ideone-parse-simple (ideone-invoke "testFunction"))
			   "error")
		     "OK"))
      (error "Auth Error"))
  (ideone-get-languages) t)

(defun ideone-set-submission-lang (&optional lang)
  (interactive "Mlang: ")
  (setq ideone-submit-lang lang))

(defun ideone-hook ()
  (local-set-key (kbd "C-c I s") 'ideone-create-submission)
  (local-set-key (kbd "C-c I l") 'ideone-set-submission-lang)
  (local-set-key (kbd "C-c I g") 'ideone-get-submission)
  ;; reserved for future
  ;;(local-set-key (kbd "C-c I u") 'ideone-update-submission)
  )

(add-hook 'find-file-hook 'ideone-hook)

(provide 'ideone)

;;; ideone.el ends here
