;;; bbdb-pilot.el --- synchronizes BBDB database to Palm Pilot.

;; Copyright (C) 1999 Jamie Zawinski <jwz@jwz.org>
;; Copyright (C) 2005 Stefan D. Bruda <bruda@cs.ubishops.ca>

(defvar bbdb-pilot-version "2.4"
  "bbdb-pilot version number"
)

;; This effort starts from the code from `bbdb-pilot-jwz.el' by Jamie
;; Zawinski <jwz@jwz.org> and Noah Friedman <friedman@splode.com>.  We
;; clean the existing code a bit and add functionality to import pilot
;; contacts into the BBDB database.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is part of the `emacs-pilot' package
;; <http://turing.ubishops.ca/home/bruda/emacs-pilot/>.  For detailed
;; usage information please refer to the README file included in the
;; package or accessible directly at
;; <http://turing.ubishops.ca/home/bruda/emacs-pilot/emacs-pilot/README>
;;
;; This code is intended to be used from within the pilot package and
;; is loaded from within the `pilot.el' file.  You can use it however
;; in a stand-alone manner (case in which you also need `pilot.el' and
;; possibly the `pilot-wait' and `pilot-wait-end' shell scripts).  The
;; function that does the transfer from the BBDB database to the pilot
;; address book is `bbdb-pilot-to-pilot', and the function that
;; integrates the new information from the pilot into the BBDB
;; database is `bbdb-pilot-to-bbdb'.
;;
;; pilot-addresses expects a file with the following 20 fields:
;;
;;  0   Last Name
;;  1   First Name
;;  2   Title
;;  3   Company
;;  4   Named Field 1 (default: Work)
;;  6   Named Field 2 (default: Home)
;;  8   Named Field 3 (default: Fax)
;; 10   Named Field 4 (default: Other)
;; 12   Named Field 5 (default: E-mail)
;; 14   Address
;; 15   City
;; 16   State
;; 17   Code
;; 18   Country
;; 19   Custom 1
;; 20   Custom 2
;; 21   Custom 3
;; 22   Custom 4
;; 23   Note
;; 24   Private  ("0" or "1")
;;
;; The "named fields" are the ones that have a field title that can be set
;; with a popup menu.  The available titles are:
;;
;;    Work
;;    Home
;;    Fax
;;    Other
;;    E-mail
;;    Main
;;    Pager
;;    Mobile
;;
;; A record in the file consists of 20 fields followed by a newline.
;; Field values are enclosed in double-quotes and are separated by
;; commas.  The "named" fields may also be preceeded by the field name
;; and a semicolon, e.g.: "Home";"(415) 555-1212",
;;
;; Strings may contain newlines, and are read with backslash-decoding
;; (for \n, \t and so on.)
;;
;; Embedded quotes are double-quoted in csv output, e.g. " -> ""
;;
;; It appears that the records have to be preceded by empty lines to
;; be recorded correctly to the pilot (at least this is the case with
;; my combination of pilot-link and pilot).

;;; History:

;; Version 2.4:
;;  o  Fixed: Nil elements in address are not liked by BBDB but they
;;     occur as effect of certain imports.
;;  o  North American phone numbers are now compared and inserted into
;;     the BBDB database correctly.
;;  o  BBDB entries with empty names are now compared with existing
;;     entries by their company field unless `bbdb-no-duplicates-p' is
;;     non_NIL (case in which the comparison is done only on names as
;;     before).
;; Version 2.3:
;;  o  FSF Emacs compatibility fixes.
;; Version 2.2:
;;  o  Augmented documentation.
;;  o  Fixed incomplete dependencies.
;; Version 2.1:
;;  o  Sanitized namespace (all the definitions are prefixed by `bbdb-pilot-').
;;  o  Workaround for emacsen lacking `temp-directory'.
;; Version 2.0:
;;  o  Cleaned up the code a bit.
;;  o  Added an import feature from Pilot to the BBDB database.
;; Version 1.8 by Jamie Zawinski

;;; Code:

(require 'bbdb)
(require 'bbdb-com)
(require 'cl)

(provide 'bbdb-pilot)
(require 'pilot)

(defcustom bbdb-pilot-trust-pilot-labels nil
  "*If non-NIL the labes from the pilot sieric label de will be copied
as they are into the BBDB database, otherwise the pilot side labels
are considered unreliable (as is the case with my version of
pilot-link) and they will get all replaced with the generic label
`Pilot'."
  :type 'sexp
  :group 'pilot)

(defcustom bbdb-pilot-to-pilot-file (expand-file-name 
                                     "bbdb-to-pilot" 
                                     (if (fboundp 'temp-directory) (temp-directory) "/tmp"))
  "*Temporary buffer for transferring BBDB entries to the pilot"
  :type 'string
  :group 'pilot)

(defcustom bbdb-pilot-from-pilot-file (expand-file-name 
                                       "bbdb-from-pilot" 
                                       (if (fboundp 'temp-directory) (temp-directory) "/tmp"))
  "*Temporary buffer for transferring pilot entries to the BBDB database"
  :type 'string
  :group 'pilot)

(defconst bbdb-pilot-field-names
  '["Work" "Home" "Fax" "Other" "E-mail" "Main" "Pager" "Mobile"])

;; `title' is in this list since, if present, it is handled specially and
;; we do not want to duplicate it in the notes section of each entry.
;; But it's still a user-defined "notes" field as far as bbdb is concerned.
(defconst bbdb-pilot-ignored-notes
  '(mail-name mail-alias face mark-char title creation-date timestamp))


(bbdb-defstruct bbdb-pilot-
  lastname		; 1
  firstname		; 2
  title			; 3
  company		; 4
  name-1 value-1	; 5
  name-2 value-2	; 6
  name-3 value-3	; 7
  name-4 value-4	; 8
  name-5 value-5	; 9
  address		; 10
  city			; 11
  state			; 12
  zip			; 13
  country		; 14
  custom-1		; 15
  custom-2		; 16
  custom-3		; 17
  custom-4		; 18
  note			; 19
  private               ; 20
  )


(defun bbdb-pilot-format (pilot)
  "Inserts a `pilot-addresses'-compatible description of the `pilot' struct
into the current buffer."
  (let ((print-escape-newlines nil)
        ;;(print-escape-nonascii nil)
        (standard-output (current-buffer)))
    (save-restriction
      (narrow-to-region (point) (point))
      (insert "\n")
      (prin1 (or (bbdb-pilot-lastname  pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-firstname pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-title     pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-company   pilot) ""))      (insert ",")

      (prin1 (or (bbdb-pilot-name-1    pilot) "Other")) (insert ";")
      (prin1 (or (bbdb-pilot-value-1   pilot) ""))      (insert ",")

      (prin1 (or (bbdb-pilot-name-2    pilot) "Other")) (insert ";")
      (prin1 (or (bbdb-pilot-value-2   pilot) ""))      (insert ",")

      (prin1 (or (bbdb-pilot-name-3    pilot) "Other")) (insert ";")
      (prin1 (or (bbdb-pilot-value-3   pilot) ""))      (insert ",")

      (prin1 (or (bbdb-pilot-name-4    pilot) "Other")) (insert ";")
      (prin1 (or (bbdb-pilot-value-4   pilot) ""))      (insert ",")

      (prin1 (or (bbdb-pilot-name-5    pilot) "Other")) (insert ";")
      (prin1 (or (bbdb-pilot-value-5   pilot) ""))      (insert ",")

      (prin1 (or (bbdb-pilot-address   pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-city      pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-state     pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-zip       pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-country   pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-custom-1  pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-custom-2  pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-custom-3  pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-custom-4  pilot) ""))      (insert ",")
      (prin1 (or (bbdb-pilot-note      pilot) ""))      
      (insert ",\"0\"")
      (insert "\n")

      ;; Replace escaped double quotes (\") with "".
      (goto-char (point-min))
      (while (re-search-forward "\\\\\"" nil t)
        (replace-match "\"\"" nil t))
      (goto-char (point-max))))
  nil)


(defun bbdb-pilot-pretty-print (pilot)
  ;; for debugging
  (let ((i 0)
        (names '["lastname" "firstname" "title" "company"
                 "name-1" "value-1" "name-2" "value-2" "name-3" "value-3"
                 "name-4" "value-4" "name-5" "value-5" "address" "city"
                 "state" "zip" "country" "custom-1" "custom-2" "custom-3"
                 "custom-4" "note"]))
    (while (< i bbdb-pilot-length)
      (insert (format "%12s: " (aref names i)))
      (let ((s (aref pilot i))
            (print-escape-newlines t))
        (if (null s) (setq s ""))
        (insert (format "%S\n" s)))
      (setq i (1+ i))))
  nil)


(defun bbdb-pilot-record-to-pilot-record (record)
  "Converts a BBDB record to a pilot record and returns the result."
  (let ((pilot (make-vector bbdb-pilot-length nil))
        (phones (bbdb-record-phones record))
        (notes (bbdb-record-raw-notes record)))

    (if (stringp notes)
        (setq notes (list (cons 'notes notes)))
      ;; may be destructively modified later
      (setq notes (copy-alist notes)))

    (if (bbdb-record-aka record)
        (setq notes
              (append notes
                      (list (cons 'AKA
                                  (mapconcat 'identity
                                             (bbdb-record-aka record)
                                             ",\n"))))))

    ;; These fields are easy...
    ;;
    (bbdb-pilot-set-lastname  pilot (bbdb-record-lastname record))
    (bbdb-pilot-set-firstname pilot (bbdb-record-firstname record))
    (bbdb-pilot-set-title     pilot (bbdb-record-getprop record 'title))
    (bbdb-pilot-set-company   pilot (bbdb-record-company record))

    ;; Now do the phone numbers...
    ;;
    (let ((pilot-phones '()))
      (while phones
        (let ((loc (bbdb-phone-location (car phones)))
              (num (bbdb-phone-string (car phones)))
              field)
          (unless loc (setq loc ""))
          (cond ((string-match "\\b\\(work\\|school\\|office\\)" loc)
                 (setq field "Work"))
                ((string-match "\\b\\(home\\)" loc)
                 (setq field "Home"))
                ((string-match "\\b\\(fax\\|facs?imile\\)" loc)
                 (setq field "Fax"))
                ((string-match "\\b\\(pager?\\|beeper\\)" loc)
                 (setq field "Pager"))
                ((string-match "\\b\\(cell\\|mobile\\)" loc)
                 (setq field "Mobile"))
                ((string-match "\\b\\(voice\\|main\\|phone\\)\\b" loc)
                 (setq field "Main"))
                ((equal loc "")
                 (setq field "Other"))
                (t
                 ;; If we don't recognise the phone label, then call it
                 ;; "Other" but preserve the original label in the field
                 ;; itself.
                 (setq field "Other"
                       num (concat loc ": " num))))

          ;; If this phone number is the same type as one previously seen
          ;; (e.g. there are two pager numbers), append with a newline to
          ;; the existing entry.  This makes it possible to group multiple
          ;; numbers in the same pilot field and make room for more numbers
          ;; of different loc types.
          (let ((seen (assoc field pilot-phones)))
            (if seen
                (setcdr seen (concat (cdr seen) "\n" num))
              (setq pilot-phones (cons (cons field num) pilot-phones))))
          (setq phones (cdr phones))))
      (setq pilot-phones (nreverse pilot-phones))

      ;; The email field goes last in the list of phone fields
      (if (bbdb-record-net record)
          (let ((c (cons "E-mail" (car (bbdb-record-net record)))))
            (setq pilot-phones (nconc pilot-phones (list c)))))

      (if (cdr (bbdb-record-net record))
          (setq notes
                (cons (cons 'other-email
                            (mapconcat 'identity
                                       (cdr (bbdb-record-net record))
                                       ",\n"))
                      notes)))

      (let (pp)
        (setq pp (pop pilot-phones))
        (bbdb-pilot-set-name-1  pilot (car pp))
        (bbdb-pilot-set-value-1 pilot (cdr pp))

        (setq pp (pop pilot-phones))
        (bbdb-pilot-set-name-2  pilot (car pp))
        (bbdb-pilot-set-value-2 pilot (cdr pp))

        (setq pp (pop pilot-phones))
        (bbdb-pilot-set-name-3  pilot (car pp))
        (bbdb-pilot-set-value-3 pilot (cdr pp))

        ;; We've filled in three phone-number fields.
        ;; If there are more than 2 phone numbers left (not counting the
        ;; email field), put remaining numbers in 4th field (with their
        ;; headings) and put the email address in the 5th field.
        (cond ((< (length pilot-phones) 3)
               (setq pp (pop pilot-phones))
               (bbdb-pilot-set-name-4  pilot (car pp))
               (bbdb-pilot-set-value-4 pilot (cdr pp))

               (setq pp (pop pilot-phones))
               (bbdb-pilot-set-name-5  pilot (car pp))
               (bbdb-pilot-set-value-5 pilot (cdr pp)))
              (t
               (let* ((email (assoc "E-mail" pilot-phones))
                      (val
                       (mapconcat
                        #'(lambda (pp)
                            (let ((p 0) s)
                              ;; If there are newlines in the data, make sure
                              ;; each new line begins with the field name
                              ;; since this record is heterogenous.
                              (while (string-match "\n" (cdr pp) p)
                                (setq s (concat "\n" (car pp) ": "))
                                (setq p (+ (match-end 0) (length s)))
                                (setcdr pp (replace-match s t t (cdr pp)))))
                            (concat (car pp) ": " (cdr pp)))
                        (delq email pilot-phones) "\n")))
                 (bbdb-pilot-set-name-4  pilot "Other")
                 (bbdb-pilot-set-value-4 pilot val)

                 (bbdb-pilot-set-name-5  pilot (car email))
                 (bbdb-pilot-set-value-5 pilot (cdr email)))))))

    ;; Now do the addresses...
    ;; Put the first address in the address field, and the others
    ;; in the "custom" fields.
    ;;
    (let* ((addrs (bbdb-record-addresses record))
           (addr1 (pop addrs)))
      (cond
       (addr1
 	(let (st)
 	  (cond ((>= bbdb-file-format 6)
                 (setq st (bbdb-join (bbdb-address-streets addr1) "\n")))
                (t
                 (setq st (bbdb-address-street1 addr1))
                 (if (> (length (bbdb-address-street2 addr1)) 0)
                     (setq st (concat st "\n" (bbdb-address-street2 addr1))))
                 (if (> (length (bbdb-address-street3 addr1)) 0)
                     (setq st (concat st "\n" (bbdb-address-street3 addr1))))))

 	  (setq st (concat (bbdb-address-location addr1) ":\n" st))

 	  (bbdb-pilot-set-address pilot st)
 	  (bbdb-pilot-set-city    pilot (bbdb-address-city  addr1))
 	  (bbdb-pilot-set-state   pilot (bbdb-address-state addr1))
 	  (bbdb-pilot-set-zip     pilot (bbdb-address-zip-string addr1))
 	  (bbdb-pilot-set-country pilot (bbdb-address-country addr1)))))

      (cond
       (addrs
        (let ((indent-tabs-mode nil)
              (formatted '())
              addr c s)
          (while addrs
            (setq addr (car addrs))
            (save-excursion
              (with-temp-buffer
                ;;(set-buffer (get-buffer-create "*bbdb-tmp*"))
                (erase-buffer)
                (insert (bbdb-address-location addr) ":\n")
                (cond
                 ((>= bbdb-file-format 6)
                  (let ((sts (bbdb-address-streets addr)))
                    (while sts
                      (indent-to 8)
                      (insert (car sts) "\n")
                      (setq sts (cdr sts)))))
                 (t
                  (if (= 0 (length (setq s (bbdb-address-street1 addr)))) nil
                    (indent-to 8) (insert s "\n"))
                  (if (= 0 (length (setq s (bbdb-address-street2 addr)))) nil
                    (indent-to 8) (insert s "\n"))
                  (if (= 0 (length (setq s (bbdb-address-street3 addr)))) nil
                    (indent-to 8) (insert s "\n"))))
                (indent-to 8)
                (insert (setq c (bbdb-address-city addr)))
                (setq s (bbdb-address-state addr))
                (if (and (> (length c) 0) (> (length s) 0)) (insert ", "))
                (insert s "  ")
                (insert (bbdb-address-zip-string addr))
                
                (when (> (length (bbdb-address-country addr)) 0)
                  (insert "\n" (bbdb-address-country addr)))
                
                (setq formatted (cons (buffer-string) formatted))
                (setq addrs (cdr addrs)))
              ))
          (setq formatted (nreverse formatted))

          (bbdb-pilot-set-custom-1 pilot (pop formatted))
          (bbdb-pilot-set-custom-2 pilot (pop formatted))
          (bbdb-pilot-set-custom-3 pilot (pop formatted))
          (if (null (cdr formatted))
              (bbdb-pilot-set-custom-4 pilot (pop formatted))
            (bbdb-pilot-set-custom-4 pilot
                                     (mapconcat 'identity formatted "\n"))))))
      )

    ;; Now handle the notes...
    ;;
    (let ((losers bbdb-pilot-ignored-notes))
      (while losers
        (let ((c (assq (car losers) notes)))
          (if c (setq notes (delete c notes))))
        (setq losers (cdr losers))))

    (bbdb-pilot-set-note pilot
     (mapconcat
      #'(lambda (cons)
          (save-excursion
            (with-temp-buffer
              ;;(set-buffer (get-buffer-create "*bbdb-tmp*"))
              (erase-buffer)
              (insert (format "%s:\n%s" (car cons) (cdr cons)))
              (goto-char (point-min))
              (while (search-forward "\n" nil t)
                (replace-match "\n       " t t))
              (goto-char (point-max))
              (skip-chars-backward "\n\t ")
              (buffer-substring (point-min) (point))
              )))
      notes
      "\n\n"))

    pilot))

(defun bbdb-pilot-make-phone (location phone-string)
  (let* ((num (make-vector
               (if bbdb-north-american-phone-numbers-p
                   bbdb-phone-length
                 2)
               nil))
         (p (bbdb-parse-phone-number phone-string)))
    (bbdb-phone-set-location num location)
    (bbdb-phone-set-area num (nth 0 p)) ; euronumbers too.
    (if (= (length num) 2)
	nil
      (bbdb-phone-set-exchange  num (nth 1 p))
      (bbdb-phone-set-suffix    num (nth 2 p))
      (bbdb-phone-set-extension num (or (nth 3 p) 0)))
    num))

(defun bbdb-pilot-to-pilot-file (filename &optional records)
  "Converts RECORDS into a format suitable to be fed to `pilot-addresses'
and puts the result in FILENAME.  If RECORDS is ommitted then converts
the whole BBDB database."
  (interactive "FWrite addresses to file: ")
  (or records (setq records (bbdb-records)))
  (save-excursion
    (set-buffer (find-file-noselect filename))
    (erase-buffer)
    (let ((len (length records))
          (i 0))
      (while records
        (message "%d%%..." (/ (* 100 i) len))
        (bbdb-pilot-format (bbdb-pilot-record-to-pilot-record (car records)))
        (setq records (cdr records)
              i (1+ i))))
    (save-buffer)
    (kill-buffer (current-buffer)))
  filename)

(defun bbdb-pilot-record-from-str ()
  "Return the pilot record starting on the current line, NIL on failure to obtain such.
Places point at the end of the record on success, at the point of
failure otherwise."
  (let ((pilot (make-vector bbdb-pilot-length nil))
        (i 0) label data)
    (beginning-of-line)
    (search-forward-regexp "^\"[^\"]*\";" (save-excursion (end-of-line) (point)) t)
    (while (< i (+ 20 5))  ;; 5 named fields!
      (if (not (search-forward-regexp "\"\\([^\"]*\\)\"\\(;\"\\([^\"]*\\)\"\\)?" nil t))
          (setq pilot nil
                i 100)
        (if (match-beginning 3)
            (setq label (buffer-substring (match-beginning 1) (match-end 1))
                  data (buffer-substring (match-beginning 3) (match-end 3)))
          (setq label ""
                data (buffer-substring (match-beginning 1) (match-end 1))))
        (when (or (equal label "") (equal label "Other"))
          (setq label nil))
        (when (equal data "")
          (setq data nil))
        (if (and (>= i 4) (< i 14))
            (progn
              (setf (aref pilot i) label)
              (setq i (+ i 1))
              (setf (aref pilot i) data))
          (setf (aref pilot i) data))
        (setq i (+ i 1))
        ))
    pilot))

(defun bbdb-pilot-record-to-bbdb-record (pilot)
  "Converts a pilot record to a BBDB record and returns the result."
  (let ((firstname (bbdb-pilot-firstname pilot))
        (lastname  (bbdb-pilot-lastname pilot))
        (company   (bbdb-pilot-company pilot))
        (title     (bbdb-pilot-title pilot))
        (aka nil)
        (net nil)
        (addrs nil)
        (phones nil)
        (pphones nil)
        (notes nil)
        (www nil)
        )
    (if (equal company "") (setq company nil))
    (if (equal title   "") (setq title nil))
    (if (equal notes   "") (setq notes nil))

    ;; Process the phone numbers and primary net address...
    ;;
    (setq pphones (list (cons (bbdb-pilot-name-1  pilot)
                              (bbdb-pilot-value-1 pilot))
                        (cons (bbdb-pilot-name-2  pilot)
                              (bbdb-pilot-value-2 pilot))
                        (cons (bbdb-pilot-name-3  pilot)
                              (bbdb-pilot-value-3 pilot))
                        (cons (bbdb-pilot-name-4  pilot)
                              (bbdb-pilot-value-4 pilot))
                        (cons (bbdb-pilot-name-5  pilot)
                              (bbdb-pilot-value-5 pilot))))
    ;; For some reason or another I do not seem to be able to rely
    ;; on pilot-addresses to return meaningful labels for the
    ;; labelled fields.  So we are just giving up on labels having
    ;; meaningful names and we parse the strings anyway.
    (while pphones
      (when (cdr (car pphones)) ;; we get empty phones for entries with fewer than 4 phone numbers!
        (cond ((and (cdr (car pphones)) (string-match "\\([^\n]*\\)\n\\(.*\\)" (cdr (car pphones))))
               ;; two phones in the same field!  We split the field and
               ;; restart the loop.
               (setq pphones (cons 
                              nil;; to get rid of...
                              (cons
                               (cons (car (car pphones)) (substring (cdr (car pphones))
                                                                    (match-beginning 1) (match-end 1)))
                               (cons
                                (cons (car (car pphones)) (substring (cdr (car pphones))
                                                                     (match-beginning 2) (match-end 2)))
                                (cdr pphones))))))
              (;;(equal (car (car pphones)) "E-mail")
               (string-match "@" (cdr (car pphones)))
               (setq net (list (cdr (car pphones)))))
              (;;(and (equal (car (car pphones)) "Other")
               (string-match "^\\([^ \t\n:]+\\):[ \t]*"
                             (cdr (car pphones)))
               (let ((a (substring (cdr (car pphones))
                                   (match-beginning 1) (match-end 1)))
                     (b (substring (cdr (car pphones)) (match-end 0))))
                 (setq phones (cons (bbdb-pilot-make-phone a b)
                                    phones)))
              )
              ((> (length (cdr (car pphones))) 0)
               (setq phones (cons (bbdb-pilot-make-phone (car (car pphones))
                                                         (cdr (car pphones)))
                                  phones)))))
      (setq pphones (cdr pphones)))
    (setq phones (nreverse phones))

    ;; Now parse the primary address...
    ;;
    (cond ((> (length (bbdb-pilot-address pilot)) 0)
           (let ((addr (make-vector bbdb-address-length nil))
                 loc sts st1 st2 st3
                 (street (bbdb-pilot-address pilot))
                 (cty (bbdb-pilot-city pilot))
                 (ste (bbdb-pilot-state pilot))
                 (zip (bbdb-pilot-zip pilot))
                 (country (bbdb-pilot-country pilot))
                 )
             (if (equal cty "") (setq cty nil))
             (if (equal ste "") (setq ste nil))
             (if (equal zip "") (setq zip nil))
             (if (equal country "") (setq country nil))
             (if zip (setq zip (bbdb-parse-zip-string zip)))

             (if (string-match "^\\([^ \t\n:]*\\):[ \t\n]*" street)
                 (setq loc (substring street 0 (match-end 1))
                       street (substring street (match-end 0))))

             (bbdb-address-set-location addr loc)

             (cond
              ((>= bbdb-file-format 6)
               (while (string-match "^\\([^\n]+\\)\\(\n\\|$\\)" street)
                 (setq sts (append
                            sts
                            (list (substring street 0 (match-end 1))))
                       street (substring street (match-end 0))))
               (bbdb-address-set-streets  addr sts))
              (t
               (if (string-match "^\\([^\n]+\\)\\(\n\\|$\\)" street)
                   (setq st1 (substring street 0 (match-end 1))
                         street (substring street (match-end 0))))
               (if (string-match "^\\([^\n]+\\)\\(\n\\|$\\)" street)
                   (setq st2 (substring street 0 (match-end 1))
                         street (substring street (match-end 0))))
               (if (string-match "^\\([^\n]+\\)\\(\n\\|$\\)" street)
                   (setq st3 (substring street 0 (match-end 1))
                         street (substring street (match-end 0))))
               (bbdb-address-set-street1  addr (or st1 ""))
               (bbdb-address-set-street2  addr (or st2 ""))
               (bbdb-address-set-street3  addr (or st3 ""))))

             (bbdb-address-set-city     addr (or cty ""))
             (bbdb-address-set-state    addr (or ste ""))
             (bbdb-address-set-country  addr (or country ""))
             (bbdb-address-set-zip      addr zip)
             (setq addrs (list addr))
             )))

    ;; Now parse the secondary addresses...
    ;;
    (let ((paddrs (list (bbdb-pilot-custom-1 pilot)
                        (bbdb-pilot-custom-2 pilot)
                        (bbdb-pilot-custom-3 pilot)
                        (bbdb-pilot-custom-4 pilot))))
      (while paddrs
        (cond
         ((car paddrs)
          ;; we are guessing here: 
          ;; second to last line is City, Province code (last two optional)
          ;; last line is country (optional!)
          ;; the other lines are street address
          (let ((paddr (car paddrs))
                lines
                (addr (make-vector bbdb-address-length nil))
                label st1 st2 st3
                (city-line 0)
                cty prov code country)
            ;; first line is the label
            (string-match "^ *\\([^:]*\\): *\n *" paddr)
            (setq label (substring paddr (match-beginning 1) (match-end 1))
                  paddr (substring paddr (match-end 0)))
            (bbdb-address-set-location addr label)
            ;; split into lines:
            (while (string-match "^ *\\([^\n]*\\)\n *\\(.*\\)$" paddr)
              (setq lines (cons (substring paddr (match-beginning 1) (match-end 1))
                                lines))
              (when (match-beginning 2)
                (setq paddr (substring paddr (match-beginning 2))))
              )
            (setq lines (cons paddr lines))
            ;; lines in reverse order, just as well

            (if (<= (length lines) 1)
                ;; one line, we take it as address line with no city or country
                (setq city-line 2)
              ;; try to find a city line between the last two lines by
              ;; looking for a postal code at the end
              (dolist (paddr (list (car lines) (cadr lines)))
                (if (string-match "^ *$" paddr)
                    ;; no city/country as per BBDB import
                    (setq city-line 2)
                  (let ((x1 "") (x2 "") (paddr paddr))
                    (while (string-match "^\\(.*\\) \\([^ ]*\\)$" paddr)
                      (setq x1 (substring paddr (match-beginning 1) (match-end 1))
                            x2 (concat (substring paddr (match-beginning 2) (match-end 2)) " " x2))
                      (cond ((string-match "  $" paddr)
                             ;; two blanks at the end is BBDB's hint for city
                             ;; line with no postal code or province/state
                             (setq cty (substring paddr 0 (match-beginning 0))
                                   paddr "")
                             (return))
                            ((and 
                              (> (length x2) 0)
                              (let (b)
                                (dolist (r bbdb-legal-zip-codes b)
                                  (when (string-match r x2) (setq b t))))
                              )
                             (setq code x2
                                   paddr x1)
                             (if (string-match "^\\([^,]*\\), \\([^,]*\\)$" paddr)
                                 ;; have a province too!
                                 (setq cty (substring paddr (match-beginning 1) (match-end 1))
                                       prov (substring paddr (match-beginning 2) (match-end 2)))
                               (setq cty x1))
                             (setq paddr "")
                             (return))
                            (t (setq paddr x1)))))
                  (setq city-line (+ city-line 1))
                  )
                )
              )
            (cond ((= city-line 0)
                   ;; no country
                   (setq lines (cdr lines))
                   )
                  ((= city-line 1)
                   ;; country + city line
                   (setq country (car lines)
                         lines (cdr (cdr lines)))
                   )
                  (t
                   ;; no apparent city line, really guessing here
                   (cond ((> (length lines) 2)
                          ;; we guess address + city + country if we have enough lines
                          (setq country (car lines)
                                cty (car (cdr lines))
                                lines (cdr (cdr lines))))
                         ((> (length lines) 1)
                          ;; otherwise we guess address + city if we have enough lines
                          (setq cty (car lines)
                                lines (cdr lines)))
                         ;; otherwise we assume no city and no country
                         )))
            (setq lines (nreverse lines))

            (when (equal cty "") (setq cty nil))
            (when (equal prov "") (setq prov nil))
            (when (equal code "") (setq code nil))
            (when (equal country "") (setq country nil))
            (when (equal code "") (setq code nil))

            (cond
             ((>= bbdb-file-format 6)
              (bbdb-address-set-streets  addr lines))
             (t
              (when (>= (length lines) 1)
                (setq st1 (nth 0 lines)))
              (when (>= (length lines) 2)
                (setq st2 (nth 1 lines)))
              (when (>= (length lines) 3)
                (setq st1 (nth 2 lines)))
              (bbdb-address-set-street1  addr (or st1 ""))
              (bbdb-address-set-street2  addr (or st2 ""))
              (bbdb-address-set-street3  addr (or st3 ""))))

            (bbdb-address-set-city     addr (or cty ""))
            (bbdb-address-set-state    addr (or prov ""))
            (bbdb-address-set-zip      addr (or code ""))
            (bbdb-address-set-country  addr (or country ""))

            (setq addrs (cons addr addrs))
            )))
        (setq paddrs (cdr paddrs))))
    (setq addrs (nreverse addrs))

    ;; Now parse the notes field.

    (when (bbdb-pilot-note pilot)
      (with-temp-buffer
        (insert (bbdb-pilot-note pilot))
        (beginning-of-buffer)
        (while (search-forward-regexp "^\n*\\([^:]*\\):\n" nil t)
          (let (label lines there)
            (setq label (buffer-substring (match-beginning 1) (match-end 1)))
            (save-excursion
              (if (search-forward-regexp "^\\([^:]*\\):\n" nil t)
                  (progn (forward-line -1)
                         (setq there (point)))
                (setq there (point-max))
                ))
            (while (search-forward-regexp "^ +\\(.*\\)$" there t)
              (setq lines (cons (buffer-substring (match-beginning 1) (match-end 1))
                                lines))
              (forward-line 1)
              )
            (setq lines (nreverse lines))
            (cond ((equal label "www")
                    (setq www (car lines)))
                  ((or (equal label "AKA") (equal label "aka"))
                   (setq aka (mapcar #'(lambda (l) (replace-in-string l "," "")) lines)))
                  ((equal label "other-email")
                   (setq net (cons (car net) 
                                   (mapcar #'(lambda (l) (replace-in-string l "," "")) lines))))
                  ((equal label "notes")
                   (dolist (x lines) (setq notes (concat notes "\n" x)))
                   (setq notes (substring notes 1)))
                  (t;; we don't know what they are, so we append to notes (including the label)
                   (setq notes (concat notes "(" label ")"))
                   (dolist (x lines) (setq notes (concat notes "\n   " x)))
                   )
                  )
            ))
        ))

    (let* ((props (cond ((and notes www) (list (cons 'notes notes) (cons 'www www)))
                        (notes (list (cons 'notes notes)))
                        (www (list (cons 'www www)))
                        (t nil)))
           (record (vector firstname lastname aka company phones addrs net props
                           (make-vector bbdb-cache-length nil))))
      record)))

(defun bbdb-pilot-find-bbdb-entry (pilot)
  "Returns the list of entries from the BBDB database matching PILOT.
Matching means here having identical name and surname if
`bbdb-no-duplicates-p' is non-NIL; otherwise the meaning is identical
name and surname, or name and surname empty and identical company."
  (mapcan #'(lambda (x)
              ;; same name and surname means same entry
              ;; nil and "" are equal
              (let ((pilot-name-0 (if (aref pilot 0) (aref pilot 0) ""))
                    (pilot-name-1 (if (aref pilot 1) (aref pilot 1) ""))
                    (x-name-0 (if (aref x 0) (aref x 0) ""))
                    (x-name-1 (if (aref x 1) (aref x 1) ""))
                    (pilot-company (if (aref pilot 3) (aref pilot 3) ""))
                    (x-company (if (aref x 3) (aref x 3) ""))
                    )
                (when (and (equal pilot-name-0 x-name-0)
                           (equal pilot-name-1 x-name-1))
                  (if (or (not (equal x-name-0 ""))
                          (not (equal x-name-1 "")))
                      (list x)
                    (when (or bbdb-no-duplicates-p (equal pilot-company x-company))
                      (list x))))))
          (bbdb-records)))

(defun bbdb-pilot-buffer-to-bbdb ()
  "Gets the contacts from the current buffer and integrates them into the BBDB database.
The current buffer must have the form of a file filled in by a call to
`pilot-addresses'.  Entries with the same name and surname as existing
entries in the BBDB database are augmented with the pilot information,
the others are inserted as new entries.  Since pilot labels are deemed
unreliable if `bbdb-pilot-trust-pilot-labels' is NIL, all the new
entries have the unique label `Pilot' in this case."
  (beginning-of-buffer)
  (let (new-entries changed-entries pilot-rec-str)
    (while (setq pilot-rec-str (bbdb-pilot-record-from-str))
      (let* ((pilot-rec (bbdb-pilot-record-to-bbdb-record pilot-rec-str))
             (bbdb-recs (bbdb-pilot-find-bbdb-entry pilot-rec))
             (bbdb-rec (when bbdb-recs (car bbdb-recs)))
             global-modified-p)
        (forward-line 1)
        (cond ((not bbdb-rec) ;; that's easy, new entry
               (setq new-entries (cons pilot-rec new-entries))
               )
              (t  ;; have entry
               ;; (aref pilot-rec 2) = AKA
               (let ((ret (bbdb-record-aka bbdb-rec)) modified-p)
                 (dolist (x (bbdb-record-aka pilot-rec))
                   (unless (member x (bbdb-record-aka bbdb-rec))
                     (setq ret (cons x ret)
                           modified-p t
                           global-modified-p t)
                     ))
                 (when modified-p
                   (bbdb-record-set-aka bbdb-rec ret)
                 ))
               ;; (aref pilot-rec 3) = company
               ;; pilot overrides BBDB unless pilot entry empty
               (let (ret)
                 (when (and (bbdb-record-company pilot-rec)
                            (not (equal (bbdb-record-company pilot-rec)
                                        (bbdb-record-company bbdb-rec))))
                   (setq ret (bbdb-record-company pilot-rec)
                         global-modified-p t)
                   (bbdb-record-set-company bbdb-rec ret)
                   ))
               ;; (aref pilot-rec 4) = phones
               (let ((ret (bbdb-record-phones bbdb-rec)) modified-p)
                 (dolist (xp (bbdb-record-phones pilot-rec))
                   (unless (let (found) 
                             (dolist (xb (bbdb-record-phones bbdb-rec) found)
                               ; broken with north american phones!
                               ;(when (equal (aref xp 1) (aref xb 1)) (setq found t))
                               (when (= (length xp) (length xb))
                                 (let ((i 1) (found1 t))
                                   (while (< i (length xp))
                                     (unless (equal (aref xp i) (aref xb i))
                                       (setq found1 nil))
                                     (setq i (+ i 1)))
                                   (when found1 (setq found t))))
                               ))
                     (unless bbdb-pilot-trust-pilot-labels
                       (setf (aref xp 0) "Pilot"))
                     (setq ret (cons xp ret)
                           modified-p t
                           global-modified-p t)
                     ))
                 (when modified-p
                   (bbdb-record-set-phones bbdb-rec ret)
                   ))
               ;; (aref pilot-rec 5) = addrs
               (let ((ret (bbdb-record-addresses bbdb-rec)) modified-p)
                 (dolist (xp (bbdb-record-addresses pilot-rec))
                   (unless (let (found) 
                             (dolist (xb (bbdb-record-addresses bbdb-rec) found)
                               (when (equal (aref xp 1) (aref xb 1)) (setq found t))))
                     (unless bbdb-pilot-trust-pilot-labels
                       (setf (aref xp 0) "Pilot"))
                     (setq ret (cons xp ret)
                           modified-p t
                           global-modified-p t)
                     ))
                 (when modified-p
                   (bbdb-record-set-addresses bbdb-rec ret)
                   ))
               ;; (aref pilot-rec 6) = net
               (let ((ret (bbdb-record-net bbdb-rec)) modified-p)
                 (dolist (xp (bbdb-record-net pilot-rec))
                   (unless (member xp (bbdb-record-net bbdb-rec))
                     (setq ret (cons xp ret)
                           modified-p t
                           global-modified-p t)))
                 (when modified-p
                   (bbdb-record-set-net bbdb-rec ret)
                   ))
               ;; (aref pilot-rec 7) = notes + www
               (when (and (bbdb-record-notes pilot-rec)
                          (not (equal (bbdb-record-notes pilot-rec) (bbdb-record-notes bbdb-rec))))
                 (if (bbdb-record-notes bbdb-rec)
                     (bbdb-record-set-notes bbdb-rec (concat (bbdb-record-notes pilot-rec) "\n" 
                                                             (bbdb-record-notes bbdb-rec)))
                   (bbdb-record-set-notes bbdb-rec (bbdb-record-notes pilot-rec)))
                 (setq global-modified-p t)
                 )
               (when (and (bbdb-record-getprop pilot-rec 'www)
                          (not (equal (bbdb-record-getprop pilot-rec 'www) 
                                      (bbdb-record-getprop bbdb-rec 'www))))
                 (bbdb-record-putprop bbdb-rec 'www (bbdb-record-getprop pilot-rec 'www))
                 (setq global-modified-p t)
                 )
               ;; (aref pilot-rec 8) = cache -- don't need to do anything
               ;; (setf (aref pilot-rec 8) (make-vector bbdb-cache-length nil))
               (when global-modified-p (setq changed-entries (cons bbdb-rec changed-entries)))
               )
              )
        )
      (if (fboundp 'line-number)
          (message "%d%%..." (/ (* 100.0 (line-number)) (line-number (point-max))))
        (message "%d%%..." (/ (* 100.0 (count-lines 1 (point))) (count-lines 1 (point-max))))
        )
      )

    ;; set changed entries:
    (dolist (rec changed-entries)
      ;; nil entries in address ungood
      (dolist (x (bbdb-record-addresses rec))
        (let ((i 2))
          (while (< i (length x))
            (unless (aref x i)
              (setf (aref x i) ""))
            (setq i (+ i 1)))
          ))
      (bbdb-change-record rec nil))

    ;; insert the new entries
    (dolist (rec new-entries)
      ;; nil entries in address ungood
      (dolist (x (bbdb-record-addresses rec))
        (let ((i 2))
          (while (< i (length x))
            (unless (aref x i)
              (setf (aref x i) ""))
            (setq i (+ i 1)))
          ))
      ;; from `bbdb-create-internal' (bbdb-com.el)
      (bbdb-invoke-hook 'bbdb-create-hook rec)
      (bbdb-change-record rec t)
      )
    (if (or new-entries changed-entries)
        (progn
          (bbdb-display-records (nconc new-entries changed-entries))
          (message "The records shown have been changed."))
      (message "Your BBDB database is already up to date."))
    ))

;;;###autoload
(defun bbdb-pilot-to-bbdb ()
  "Gets the pilot address book and inserts the new entries into the BBDB database.
Also augments the existing BBDB entries whenever extra information is
available for them on the pilot side."
  (interactive)
  (pilot-run-command (format "pilot-addresses -p %s -a -w %s" 
                             pilot-device bbdb-pilot-from-pilot-file)
                     "Press the HotSync button now to import contacts into the BBDB database.")
  (let ((buf (set-buffer (find-file-noselect bbdb-pilot-from-pilot-file))))
    (bbdb-pilot-buffer-to-bbdb)
    (kill-buffer buf)))

;;;###autoload
(defun bbdb-pilot-to-pilot ()
  "Pushes the current contents of BBDB out to the pilot."
  (interactive)
  (bbdb-records)  ; load bbdb
  (message "Selecting records...")
  (let ((records
         (remove-if-not
          #'(lambda (record)
              (or (bbdb-record-name record)
                  (bbdb-record-company record))
              )
          (bbdb-records))))

    (save-excursion
      (bbdb-pilot-to-pilot-file bbdb-pilot-to-pilot-file records)
      (pilot-run-command (format "pilot-addresses -p %s  -d BBDB -c BBDB -r %s" 
                                 pilot-device bbdb-pilot-to-pilot-file)
                         "Press the HotSync button now to export the BBDB database.")
      )))

;;; end of bbdb-pilot.el
