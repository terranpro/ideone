-*- mode: org -*-

* IDEone for Emacs!
ideone.el is a simple wrapper for IDEone's SOAP API that works using
emacs soap client.  IDEone is an online code paste site which features
sophisticated compiler sandbox'ing (i.e. you can run code and get
results)!  It features a wide variety of languages, and has become a
standard for snippet sharing on places like stackoverflow and
IRC #c++@Freenode.

ideone.el provides a way to work with IDEone directly from Emacs.  It
allows you to post code in any of the IDEone supported languages,
compile, see results and easily share the code with others.  It also
allows you to easily wisk in IDEone snippets from others' with a
keystroke, then analyze, modify, and re-submit.  

Due to current limitations of the soap client, the provided WSDL is
slightly modified to work with it, and distributed together.

** Author
Brian Fransioli (assem <AT> terranpro.org)

* Features
- Submission Creation
- Submission Retrieval (and modification!)
- Recent Submissions

* Installation
** Obtaining
git clone git://github.com/terranpro/ideone.git ideone

** Emacs Configuration
(load-library "~/elisp/ideone/ideone.el")

(setq ideone-user "IDEONE_USERID")
(setq ideone-pass "IDEONE_API_PASSWD")

(ideone-init)

;; If you encounter problems try this:
;;(ideone-enable-debug)

* Usage
** Command Reference
*** M-x ideone-get-submission
Attempt to retrieve an IDEone url at point, else request the user
input a valid IDEone address of either form:

http://ideone.com/45AzL

or

45AzL

*** M-x ideone-create-submission
For the current buffer (or active region inside current buffer),
create a submission using the currently active, buffer settings.
These include:
 - language (if not set, will guess using major mode)
 - compile T/F
 - input (TODO)

*** M-x ideone-set-submission-lang
Perform a completing read from the user on the selection of submission
language.  The user can choose from all of the supported IDEone
languages.

** Keystrokes Reference
|-----------+-------------------|
| =C-c I g= | Get Submission    |
| =C-c I s= | Create Submission |
|-----------+-------------------|

* Feedback
All suggestions, comments, bugs, or complaints to (assem AT
terranpro.org) or contact through github.

Enjoy!
