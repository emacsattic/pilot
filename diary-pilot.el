;;; diary-pilot.el --- synchronizes Emacs diary to Palm Pilot.

;; Copyright (C) 2000, 2001 Martin Schwenke
;; Copyright (C) 2005 Stefan D Bruda <bruda@cs.ubishops.ca>

(defvar diary-pilot-version "2.4"
  "diary-pilot version number")

;; Original author: Martin Schwenke <martin@meltin.net>.  Loosely
;; based on Jamie Zawinski's bbdb-pilot.el.  Some of the code is based
;; on bits of diary-lib.el from Emacs 20.6.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is part of the `emacs-pilot' package
;; <http://turing.ubishops.ca/home/bruda/emacs-pilot/>.  For detailed
;; usage information please refer to the README file included in the
;; package or accessible directly at
;; <http://turing.ubishops.ca/home/bruda/emacs-pilot/emacs-pilot/README>
;;
;; This program installs all of the events from the Emacs diary for
;; the next N days (default 365) to the pilot datebook and then
;; removes duplicates from the pilot datebook.  Finally, it gets the
;; new events from the pilot and inserts them into the Emacs diary.
;; For this purpose, events are considered identical iff their text
;; and their time are identical; only one-time events are supported.

;; This code is intended to be used from within the pilot package and
;; is from within the `pilot.el' file.  You can use it however in a
;; stand-alone manner (case in which you also need the mentioned
;; `pilot.el' and the `pilot-wait' and `pilot-wait-end' shell
;; scripts).  The function that does the back and forth transfer
;; between the Emacs diary and the pilot is `diary-pilot-to-pilot'.

;;; History:

;; Version 2.4:
;;  o  Cyclic appointments of form "first Sunday after the 15th" are
;;     now imported from the pilot using a diary S-exp.
;;  o  The UNTIL pilot clause is now supported.
;;  o  Fixed other pilot to diary import issues such as failure to
;;     import appointments without a start time.
;;  o  Added support for appointment notes (see `diary-pilot-ignore-notes').
;;  o  Pilot entries far in the future are now handled correctly and
;;     inserted into the diary only if they are not there already.
;; Version 2.3:
;;  o  More compatibility fixes.
;; Version 2.2:
;;  o  A fix for a (blatant) code bug.
;;  o  Augmented documentation.
;; Version 2.1:
;;  o  Sanitized namespace (all the definitions are prefixed by `diary-pilot-').
;;  o  Workaround for emacsen lacking `temp-directory'.
;; Version 2.0:
;;  o  Cleaned up code a bit.
;;  o  Added bi-directional transfer for appointments.
;; Initial version 1.8 by Martin Schwenke.

;;; Code:

(require 'diary-lib)
(require 'calendar)
;(require 'appt)

(provide 'diary-pilot)
(require 'pilot)

(defcustom diary-pilot-numdays 365
  "*Numbers of days of diary entries to export to pilot."
  :type 'integer
  :group 'pilot)

(defcustom diary-pilot-alarm-time 10
  "*Alarm time for events exported to pilot."
  :type 'integer
  :group 'pilot)

(defcustom diary-pilot-to-pilot-file (expand-file-name 
                                      "diary-to-pilot" 
                                      (if (fboundp 'temp-directory) (temp-directory) "/tmp"))
  "*File to use for exporting diary to pilot."
  :type 'string
  :group 'pilot)

(defcustom diary-pilot-from-pilot-file (expand-file-name 
                                        "diary-from-pilot" 
                                        (if (fboundp 'temp-directory) (temp-directory) "/tmp"))
  "*File to use for getting diary from pilot."
  :type 'string
  :group 'pilot)

(defcustom diary-pilot-database "DatebookDB"
  "*Database to flush duplicates from after exporting diary to pilot."
  :type 'string
  :group 'pilot)

(defcustom diary-pilot-ignore-entries '("**pilot-exclude**")
  "*Entries containing these strings will not be transferred to the pilot."
  :type 'sexp
  :group 'pilot)

(defcustom diary-pilot-ignore-notes t
  "*If non-NIL (default) appointment notes will not be imported from the pilot.
Otherwise notes are included after the message and separated by the 
message by `--'."
  :type 'sexp
  :group 'pilot)

(defvar diary-pilot-list-entries-hook '(sort-diary-entries)
  "*List of functions to call when processing diary to export it to pilot.
See `list-diary-entries-hook'.")

(defun diary-pilot-end-time (s)
  "Similar to function `diary-entry-time', except gets end time for event.
Assumes that start and end time are given in same format."
  (let ((case-fold-search nil))
    (cond ((string-match;; Hour and minute  XX:XXam or XX:XXpm
	    (concat 
	     "^[ \t]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>"
	     "[ \t]*-[ \t]*"
	     "\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>")
	     s)
	   (+ (* 100 (% (string-to-int
			   (substring s (match-beginning 4) (match-end 4)))
			  12))
	      (string-to-int (substring s (match-beginning 5) (match-end 5)))
	      (if (equal ?a (downcase (aref s (match-beginning 6))))
		  0 1200)))
	  ((string-match;; Military time  
	    (concat 
	     "^[ \t]*\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)"
	     "[ \t]*-[ \t]*"
	     "\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)")
	    s)
	   (+ (* 100 (string-to-int
		      (substring s (match-beginning 3) (match-end 3))))
	      (string-to-int (substring s (match-beginning 4) (match-end 4)))))
	  ((string-match;; Hour only  XXam or XXpm
	    (concat
	     "^[ \t]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>"
	     "[ \t]*-[ \t]*"
	     "\\([0-9]?[0-9]\\)\\([ap]\\)m\\>")
	    s)
	   (+ (* 100 (% (string-to-int
			 (substring s (match-beginning 3) (match-end 3)))
			12))
	      (if (equal ?a (downcase (aref s (match-beginning 4))))
		  0 1200)))
	  (t diary-unknown-time))));; Unrecognizable

(defun diary-pilot-text (s)
  "Similar to function `diary-entry-time', except gets text after times.
Assumes that start and end time are given in same format."
  (let ((case-fold-search nil))
    (subst-char-in-string
     ?\t ?\040
     (cond ((string-match;; Hour and minute  XX:XXam or XX:XXpm
	     (concat 
	      "^[ \t]*\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>"
	      "\\(" "[ \t]*-[ \t]*"
	      "\\([0-9]?[0-9]\\):\\([0-9][0-9]\\)\\([ap]\\)m\\>"
	      "\\)?[ \t]*\\(.*\\)$")
	     s)
	    (substring s (match-beginning 8) (match-end 8)))
	   ((string-match;; Military time  
	     (concat 
	      "^[ \t]*\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)"
	      "\\(" "[ \t]*-[ \t]*"
	      "\\([0-9]?[0-9]\\):?\\([0-9][0-9]\\)"
	      "\\)?[ \t]*\\(.*\\)$")
	     s)
	    (substring s (match-beginning 6) (match-end 6)))
	   ((string-match;; Hour only  XXam or XXpm
	     (concat
	      "^[ \t]*\\([0-9]?[0-9]\\)\\([ap]\\)m\\>"
	      "\\(" "[ \t]*-[ \t]*"
	      "\\([0-9]?[0-9]\\)\\([ap]\\)m\\>"
	      "\\)?[ \t]*\\(.*\\)$")
	     s)
	    (substring s (match-beginning 6) (match-end 6)))
	   (t
	    s)))))

(defun diary-pilot-ignore-p (text)
  "Returns non-nil iff the text represents an ignorable diary entry
as specified by `diary-pilot-ignore-entries'."
  (let ((ret nil))
    (with-temp-buffer
      (insert text)
      (dolist (comp diary-pilot-ignore-entries ret)
        (beginning-of-buffer)
        (when (search-forward comp nil t)
          (setq ret t))
        )
      )))
  
(defun diary-pilot-format (diary)
  "Inserts an `install-datebook'-compatible description of the value
of DIARY into the current buffer."
  (let* ((calendar-date-display-form '((format "%04d/%02d/%02d"
					       (string-to-int year)
					       (string-to-int month)
					       (string-to-int day))))
	 (thang (cadr diary))
	 (date  (calendar-date-string (car diary) ))
	 (ndate (calendar-date-string
		 (calendar-gregorian-from-absolute
		  (1+ (calendar-absolute-from-gregorian (car diary))))))
	 (start (diary-entry-time thang))
	 (end   (diary-pilot-end-time thang))
	 (text  (diary-pilot-text thang))
	 (print-escape-newlines nil)
	 (startstring date)
	 (endstring   "")
	 (alarmstring "")
	 (stdalarm    (format "%dm" diary-pilot-alarm-time)))

    (if (/= start diary-unknown-time)
	(progn
	  ;; Start time is valid, incorporate into startstring.
	  (setq startstring
		(format "%s %02d:%02d" date
			(/ start 100) (% start 100)))
	  (setq alarmstring stdalarm)
	  (if (= end diary-unknown-time)
	      ;; End time is bogus/missing.  Use startstring.
	      (setq endstring startstring)
	    ;; End time is cool.  If it is greater than start time
	    ;; then things are as expected, otherwise we need to hack
	    ;; in the next day!
	    (if (> end start)
		(setq endstring
		      (format "%s %02d:%02d" date
			      (/ end 100) (% end 100)))
	      (setq endstring
		    (format "%s %02d:%02d" ndate
			    (/ end 100) (% end 100)))))))
    (unless (diary-pilot-ignore-p text)
      (insert (format "%s\t%s\t\%s\t%s\n"
                      startstring
                      endstring
                      alarmstring
                      text))
      )
    ))

(defun diary-pilot-to-pilot-file (filename)
  "Puts into FILENAME an `install-datebook'-compatible description of diary entries
for the next `diary-pilot-numdays' days."
  (interactive "FWrite install-datebook file: ")

  (message "Entries...")
  (let* ((diary-display-hook      'ignore)
	 (diary-hook              nil)
	 (list-diary-entries-hook diary-pilot-list-entries-hook)
	 (records                 (list-diary-entries
				   (calendar-current-date)
				   diary-pilot-numdays)))
    (save-excursion
      (show-all-diary-entries)
      (set-buffer (find-file-noselect filename))
      (erase-buffer)
      (let ((len (length records))
	    (i 0))
	(while records
	  (message "%d%%..." (/ (* 100 i) len))
	  (diary-pilot-format (car records))
	  (setq records (cdr records)
		i (1+ i))))
      (save-buffer)
      (kill-buffer (current-buffer)))
    (when (fboundp 'appt-initialize)
      (appt-initialize))
    filename))

(defun diary-pilot-month-to-number (month)
  "Converts MONTH (given as a three-letter string) to its numerical representation.
Retruns NIL on inexistent months."
  (and month
       (let ((months '(("jan" .  1) ("feb" .  2) ("mar" .  3) ("apr" .  4) 
                       ("may" .  5) ("jun" .  6) ("jul" .  7) ("aug" .  8) 
                       ("sep" .  9) ("oct" . 10) ("nov" . 11) ("dec" . 12) )))
         (or (cdr (assoc (downcase month) months)) nil))))

(defun diary-pilot-weekday-to-number (weekday)
  "Converts WEEKDAY (given as a three-letter string) to its numerical representation, 
where 0 = Sunday.  Retruns NIL on inexistent months."
  (and weekday
       (let ((week '(("sun" .  0) ("mon" .  1) ("tue" .  2) ("wed" .  3) 
                       ("thu" .  4) ("fri" .  5) ("sat" .  6) )))
         (or (cdr (assoc (downcase weekday) week)) nil))))

(defun diary-pilot-date-plus (date days)
  "return the date past DAYS after DATE."
  (calendar-gregorian-from-absolute (+ days (calendar-absolute-from-gregorian date))))

(defun diary-pilot-from-pilot-buffer ()
  "Parse the appointments from the current buffer and returns the parsed list.
Appointments must have the form returned by `reminders' from
`pilot-link'.  Specificaly the following forms are recognized:

REM <day> [<month> [<year>]] [*<cycle>] .* MSG <message> %a from <start> to <end>
REM <wekday> [<after-day>] .* MSG <message> %a from <start> to <end>

Returns three lists: the first is a list of appointments with precise
time and date, of form '((MONTH DAY YEAR) \" TIME-END MESSAGE\"),
where NIL is a possible wildcard for MONTH and YEAR; the second list
is a list of weekly appointments, of form '(WEEKDAY 
\" TIME-END MESSAGE\"); the third is a list of cyclic appointments, of
form '(%%(diary-cyclic CYCLE MONTH DAY YEAR) \" TIME-END MESSAGE\")."
  (beginning-of-buffer)
  (let (appts appts-wk appts-cyclic
        (there (save-excursion (end-of-line) (point))))
    (beginning-of-line)
    (while (not (equal (point) (point-max)))
      (let (day month year start-time end-time message cycle-days day-wk after-day note until)
      (when (save-excursion 
              (search-forward-regexp 
               "UNTIL \\([0-9]+\\) \\([a-z][a-z][a-z]\\) \\([0-9][0-9][0-9][0-9]\\).*MSG"
               there t))
        (setq until (list (diary-pilot-month-to-number (buffer-substring (match-beginning 2) 
                                                                         (match-end 2)))
                          (string-to-int (buffer-substring (match-beginning 1) (match-end 1))) 
                          (string-to-int (buffer-substring (match-beginning 3) (match-end 3)))
                          )))
      (cond (;; appointments with precise time
             (save-excursion
               (search-forward-regexp 
                "^REM \\([0-9]+\\) \\([a-z][a-z][a-z]\\) \\([0-9][0-9][0-9][0-9]\\) \\*\\([0-9]*\\).*MSG \\(.*\\) %a *\\(.*\\) *from \\([0-9]+\\:[0-9]+\\) to \\([0-9]+\\:[0-9]+\\).*$"
                there t))
             ;; 20 nov 2005 *10 (repeat every cycle-days-th day)
             ;; 1/day, 2/month, 3/year, 4/cycle-days, 5/message, 6/note, 7/start-time, 8/end-time
             (setq day (buffer-substring (match-beginning 1) (match-end 1))
                   month (buffer-substring (match-beginning 2) (match-end 2))
                   year (buffer-substring (match-beginning 3) (match-end 3))
                   cycle-days (buffer-substring (match-beginning 4) (match-end 4))
                   message (buffer-substring (match-beginning 5) (match-end 5))
                   note (buffer-substring (match-beginning 6) (match-end 6))
                   start-time (buffer-substring (match-beginning 7) (match-end 7))
                   end-time (buffer-substring (match-beginning 8) (match-end 8)))
             )
            ((save-excursion
               (search-forward-regexp 
            "^REM \\([0-9]+\\) \\([a-z][a-z][a-z]\\) \\([0-9][0-9][0-9][0-9]\\) .*MSG \\(.*\\) %a *\\(.*\\) *from \\([0-9]+\\:[0-9]+\\) to \\([0-9]+\\:[0-9]+\\).*$"
            there t))
             ;; 20 nov 2005
             ;; 1/day, 2/month, 3/year, 4/message, 5/note, 6/start-time, 7/end-time
             (setq day (buffer-substring (match-beginning 1) (match-end 1))
                   month (buffer-substring (match-beginning 2) (match-end 2))
                   year (buffer-substring (match-beginning 3) (match-end 3))
                   message (buffer-substring (match-beginning 4) (match-end 4))
                   note (buffer-substring (match-beginning 5) (match-end 5))
                   start-time (buffer-substring (match-beginning 6) (match-end 6))
                   end-time (buffer-substring (match-beginning 7) (match-end 7)))
             )
            ((save-excursion
               (search-forward-regexp 
                "^REM \\([0-9]+\\) \\([a-z][a-z][a-z]\\) .*MSG \\(.*\\) %a *\\(.*\\) *from \\([0-9]+\\:[0-9]+\\) to \\([0-9]+\\:[0-9]+\\).*$"
                there t))
             ;; 20 nov
             ;; 1/day, 2/month, 3/message, 4/note, 5/start-time, 6/end-time
             (setq day (buffer-substring (match-beginning 1) (match-end 1))
                   month (buffer-substring (match-beginning 2) (match-end 2))
                   message (buffer-substring (match-beginning 3) (match-end 3))
                   note (buffer-substring (match-beginning 4) (match-end 4))
                   start-time (buffer-substring (match-beginning 5) (match-end 5))
                   end-time (buffer-substring (match-beginning 6) (match-end 6)))
             )
            ((save-excursion
               (search-forward-regexp 
                "^REM \\([0-9]+\\) .*MSG \\(.*\\) %a *\\(.*\\) *from \\([0-9]+\\:[0-9]+\\) to \\([0-9]+\\:[0-9]+\\).*$"
                there t))
             ;; 20
             ;; 1/day, 2/message, 3/note, 4/start-time, 5/end-time
             (setq day (buffer-substring (match-beginning 1) (match-end 1))
                   message (buffer-substring (match-beginning 2) (match-end 2))
                   note (buffer-substring (match-beginning 3) (match-end 3))
                   start-time (buffer-substring (match-beginning 4) (match-end 4))
                   end-time (buffer-substring (match-beginning 5) (match-end 5)))
             )
            ((save-excursion
               (search-forward-regexp 
                "^REM \\([a-z][a-z][a-z]\\) \\([0-9]+\\) .*MSG \\(.*\\) %a *\\(.*\\) *from \\([0-9]+\\:[0-9]+\\) to \\([0-9]+\\:[0-9]+\\).*$"
                there t))
             ;; Sun 15
             ;; 1/day-wk, 2/after-day, 3/mesage, 4/note, 5/start-time, 6/end-time
             (setq day-wk (buffer-substring (match-beginning 1) (match-end 1))
                   after-day (buffer-substring (match-beginning 2) (match-end 2))
                   message (buffer-substring (match-beginning 3) (match-end 3))
                   note (buffer-substring (match-beginning 4) (match-end 4))
                   start-time (buffer-substring (match-beginning 5) (match-end 5))
                   end-time (buffer-substring (match-beginning 6) (match-end 6)))
             )
            ((save-excursion
               (search-forward-regexp 
                "^REM \\([a-z][a-z][a-z]\\) .*MSG \\(.*\\) %a *\\(.*\\) *from \\([0-9]+\\:[0-9]+\\) to \\([0-9]+\\:[0-9]+\\).*$"
                there t))
             ;; Sun
             ;; 1/day-wk, 2/message, 3/note, 4/start-time, 5/end-time
             (setq day-wk (buffer-substring (match-beginning 1) (match-end 1))
                   message (buffer-substring (match-beginning 2) (match-end 2))
                   note (buffer-substring (match-beginning 3) (match-end 3))
                   start-time (buffer-substring (match-beginning 4) (match-end 4))
                   end-time (buffer-substring (match-beginning 5) (match-end 5)))
             )

            ;; appointments without time
            ((save-excursion
               (search-forward-regexp 
                "^REM \\([0-9]+\\) \\([a-z][a-z][a-z]\\) \\([0-9][0-9][0-9][0-9]\\) \\*\\([0-9]*\\).*MSG \\(.*\\) %a *\\(.*\\)$"
                there t))
             ;; 20 nov 2005 *10 (repeat every cycle-days-th day)
             ;; 1/day, 2/month, 3/year, 4/cycle-days, 5/message, 6/note
             (setq day (buffer-substring (match-beginning 1) (match-end 1))
                   month (buffer-substring (match-beginning 2) (match-end 2))
                   year (buffer-substring (match-beginning 3) (match-end 3))
                   cycle-days (buffer-substring (match-beginning 4) (match-end 4))
                   message (buffer-substring (match-beginning 5) (match-end 5))
                   note (buffer-substring (match-beginning 6) (match-end 6))
                   start-time nil
                   end-time nil)
             )
            ((save-excursion
               (search-forward-regexp 
            "^REM \\([0-9]+\\) \\([a-z][a-z][a-z]\\) \\([0-9][0-9][0-9][0-9]\\) .*MSG \\(.*\\) %a *\\(.*\\)$"
            there t))
             ;; 20 nov 2005
             ;; 1/day, 2/month, 3/year, 4/message, 5/note
             (setq day (buffer-substring (match-beginning 1) (match-end 1))
                   month (buffer-substring (match-beginning 2) (match-end 2))
                   year (buffer-substring (match-beginning 3) (match-end 3))
                   message (buffer-substring (match-beginning 4) (match-end 4))
                   note (buffer-substring (match-beginning 5) (match-end 5))
                   start-time nil
                   end-time nil)
             )
            ((save-excursion
               (search-forward-regexp 
                "^REM \\([0-9]+\\) \\([a-z][a-z][a-z]\\) .*MSG \\(.*\\) %a *\\(.*\\)$"
                there t))
             ;; 20 nov
             ;; 1/day, 2/month, 3/message, 4/note
             (setq day (buffer-substring (match-beginning 1) (match-end 1))
                   month (buffer-substring (match-beginning 2) (match-end 2))
                   message (buffer-substring (match-beginning 3) (match-end 3))
                   note (buffer-substring (match-beginning 4) (match-end 4))
                   start-time nil
                   end-time nil)
             )
            ((save-excursion
               (search-forward-regexp 
                "^REM \\([0-9]+\\) .*MSG \\(.*\\) %a *\\(.*\\)$"
                there t))
             ;; 20
             ;; 1/day, 2/message, 3/note
             (setq day (buffer-substring (match-beginning 1) (match-end 1))
                   message (buffer-substring (match-beginning 2) (match-end 2))
                   note (buffer-substring (match-beginning 3) (match-end 3))
                   start-time nil
                   end-time nil)
             )
            ((save-excursion
               (search-forward-regexp 
                "^REM \\([a-z][a-z][a-z]\\) \\([0-9]+\\) .*MSG \\(.*\\) %a *\\(.*\\)$"
                there t))
             ;; Sun 15
             ;; 1/day-wk, 2/after-day, 3/mesage, 4/note
             (setq day-wk (buffer-substring (match-beginning 1) (match-end 1))
                   after-day (buffer-substring (match-beginning 2) (match-end 2))
                   message (buffer-substring (match-beginning 3) (match-end 3))
                   note (buffer-substring (match-beginning 4) (match-end 4))
                   start-time nil
                   end-time nil)
             )
            ((save-excursion
               (search-forward-regexp 
                "^REM \\([a-z][a-z][a-z]\\) .*MSG \\(.*\\) %a *\\(.*\\)$"
                there t))
             ;; Sun
             ;; 1/day-wk, 2/message, 3/note
             (setq day-wk (buffer-substring (match-beginning 1) (match-end 1))
                   message (buffer-substring (match-beginning 2) (match-end 2))
                   note (buffer-substring (match-beginning 3) (match-end 3))
                   start-time nil
                   end-time nil)
             )
            )
      (when (and (not diary-pilot-ignore-notes)
                 (not (equal note "")))
        (setq message (format "%s--%s" message note))
        )
      (cond ((and day-wk after-day) ;; first day-wk after the after-day-th
             (setq appts-cyclic
                   (cons 
                    (list 
                     (if until
                         (let ((beg (calendar-current-date)))
                           (format 
                            "%%(and (= %d (calendar-day-of-week date)) (>= (extract-calendar-day date) %s) (diary-block %s %s %s %s %s %s))"
                            (diary-pilot-weekday-to-number day-wk) after-day
                            (car beg) (cadr beg) (caddr beg) (car until) (cadr until) (caddr until) )
                           )
                       (format 
                        "%%(and (= %d (calendar-day-of-week date)) (>= (extract-calendar-day date) %s))"
                        (diary-pilot-weekday-to-number day-wk) after-day)
                       )
                     (cond 
                      ((not start-time) (format "  %s" message))
                      ((or (equal start-time end-time) (not end-time))
                       (format "  %s %s" start-time message))
                      (t (format "  %s-%s %s" start-time end-time message))
                      ))
                    appts-cyclic))
             )
           ;((and day-wk after-day) ;; first day-wk after the after-day-th
           ; ;; we don't have a mechanism for such an appt, so we
           ; ;; generate all the repeated individual appts from today
           ; ;; to today + `diary-pilot-numdays'
           ; (let* ((beg (calendar-current-date))
           ;        (end (diary-pilot-date-plus beg diary-pilot-numdays))
           ;        (day (string-to-int after-day))
           ;        (month (extract-calendar-month beg))
           ;        (year (extract-calendar-year beg))
           ;        new-date)
           ;   (while (calendar-date-compare 
           ;           (list (setq new-date 
           ;                       (calendar-gregorian-from-absolute 
           ;                        (calendar-dayname-on-or-before 
           ;                         (diary-pilot-weekday-to-number day-wk) 
           ;                         (+ 6 (calendar-absolute-from-gregorian (list month day year)))))))
           ;           (list end))
           ;     (setq appts (cons (list new-date 
           ;                             (cond 
           ;                              ((not start-time) (format "  %s" message))
           ;                              ((or (equal start-time end-time) (not end-time))
           ;                               (format "  %s %s" start-time message))
           ;                              (t (format "  %s-%s %s" start-time end-time message))
           ;                              )) appts))
           ;     (increment-calendar-month month year 1)
           ;     )))

            (day-wk ;; weekly appt, to appts-wk
             (setq appts-wk (cons (list 
                                   (if until
                                       (let ((beg (calendar-current-date)))
                                         (format
                                          "%%(and (= %d (calendar-day-of-week date)) (diary-block %s %s %s %s %s %s))"
                                          (diary-pilot-weekday-to-number day-wk) 
                                          (car beg) (cadr beg) (caddr beg) 
                                          (car until) (cadr until) (caddr until)))
                                     (downcase day-wk))
                                   (cond 
                                    ((not start-time) (format "  %s" message))
                                    ((or (equal start-time end-time) (not end-time))
                                     (format "  %s %s" start-time message))
                                    (t (format "  %s-%s %s" start-time end-time message))
                                    )
                                   )
                                  appts-wk)))
            ((and day month year cycle-days) ;; cyclic appt, every cycle-days days
             (setq appts-cyclic 
                   (cons 
                    (list 
                     (if until
                         (let ((beg (calendar-current-date)))
                           (format "%%(and (diary-cyclic %d %d %d %d) (diary-block %s %s %s %s %s %s))"
                                   (string-to-int cycle-days) 
                                   (diary-pilot-month-to-number month)
                                   (string-to-int day) 
                                   (string-to-int year)
                                   (car beg) (cadr beg) (caddr beg) 
                                   (car until) (cadr until) (caddr until))
                           )
                       (format "%%(diary-cyclic %d %d %d %d)" 
                               (string-to-int cycle-days) 
                               (diary-pilot-month-to-number month)
                               (string-to-int day) 
                               (string-to-int year)) 
                       )

                     (cond 
                      ((not start-time) (format "  %s" message))
                      ((or (equal start-time end-time) (not end-time))
                       (format "  %s %s" start-time message))
                      (t (format "  %s-%s %s" start-time end-time message))
                      )
                     )
                    appts-cyclic))
             )
            (day ;; normal date, to appts 
             (setq appts (cons (list
                                (list (diary-pilot-month-to-number month) (string-to-int day) 
                                      (and year (string-to-int year))) 
                                (cond 
                                 ((not start-time) (format "  %s" message))
                                 ((or (equal start-time end-time) (not end-time))
                                  (format "  %s %s" start-time message))
                                 (t (format "  %s-%s %s" start-time end-time message))
                                 )
                                ) appts))
             )
            ;; unknown entries, silently ignored 
            ))
      (forward-line 1)
      (beginning-of-line)
      (setq there (save-excursion (end-of-line) (point))))
    (list appts appts-wk appts-cyclic)))

(defun diary-pilot-from-pilot-file ()
  "Retrieves the appointments from the pilot and returns them as processed by `diary-pilot-from-pilot-buffer'."
  (set-buffer (find-file-noselect diary-pilot-from-pilot-file))
  (erase-buffer)
  (pilot-run-command (format "reminders -p %s" pilot-device)
                     "Press the HotSync button now to import diary." t)
  (let ((ret (diary-pilot-from-pilot-buffer)))
    (save-buffer)
    (kill-buffer (current-buffer))
    ret)
)

(defun diary-pilot-calendar-date-plus (date days)
  "Returns the date DAYS days after DATE."
  (calendar-gregorian-from-absolute (+ (calendar-absolute-from-gregorian date) days))
)

(defun diary-pilot-calendar-date-minus (date1 date2)
  "Retruns the number of days between DATE1 and DATE2."
  (- (calendar-absolute-from-gregorian date1)
     (calendar-absolute-from-gregorian date2))
)

(defun diary-pilot-find-new-pilot-records (appts appts-wk appts-cyclic)
  "Returns the records from APPTS, APPTS-WK, and APPTS-CYCLIC that are not included in the diary
and are still pertinent (i.e., their date is today or later).  Both
the returned records and the elements of APPTS, APPTS-WK, and
APPTS-CYCLIC have the same format as the records returned by
`diary-pilot-from-pilot-buffer'.  Two records are considered identical
if their text and their time are the same (irrespective of their
date)."
  (let* ((diary-pilot-numdays diary-pilot-numdays)
         (diary (list-diary-entries
                 (calendar-current-date)
                 diary-pilot-numdays))
         (today (calendar-current-date))
         new-appts)
    (dolist (appt appts)
      (let ((how-far (unless (member nil (nth 0 appt))
                       (diary-pilot-calendar-date-minus (nth 0 appt) (calendar-current-date))))
            found)
        (when (and how-far (> how-far diary-pilot-numdays))
          (setq diary-pilot-numdays (+ how-far 1)
                diary (list-diary-entries (calendar-current-date) diary-pilot-numdays)))
        (dolist (diary-appt diary)
          (let* ((date (nth 0 appt))
                 (message (nth 1 appt))
                 (start (diary-entry-time message))
                 (end (diary-pilot-end-time message))
                 (text (diary-pilot-text message))

                 (diary-message (nth 1 diary-appt))
                 (diary-start (diary-entry-time diary-message))
                 (diary-end (diary-pilot-end-time diary-message))
                 (diary-text (diary-pilot-text diary-message))
                 )
            (when (or (if (member nil date)
                          nil
                        (calendar-date-compare (list date) (list today)))
                      (and (equal start diary-start)
                           (equal end diary-end)
                           (equal text diary-text)))
              (setq found t))
            ))
        (unless found
          (setq new-appts (cons appt new-appts)))))

    (dolist (appt (append appts-wk appts-cyclic) new-appts)
      (let (found)
        (dolist (diary-appt diary)
          (let* ((message (nth 1 appt))
                 (start (diary-entry-time message))
                 (end (diary-pilot-end-time message))
                 (text (diary-pilot-text message))

                 (diary-message (nth 1 diary-appt))
                 (diary-start (diary-entry-time diary-message))
                 (diary-end (diary-pilot-end-time diary-message))
                 (diary-text (diary-pilot-text diary-message))
                 )
            (when (and (equal start diary-start)
                       (equal end diary-end)
                       (equal text diary-text))
              (setq found t))
            ))
        (unless found
          (setq new-appts (cons appt new-appts)))))
    ))

(defun diary-pilot-insert-pilot-entries ()
  "Get the pilot entries and insert them into the diary."
  (save-excursion
    (let* ((pilot-appts (diary-pilot-from-pilot-file))
           (new-entries (diary-pilot-find-new-pilot-records
                         (car pilot-appts) (cadr pilot-appts) (caddr pilot-appts)))
           (diary-buff (get-file-buffer diary-file)))
      (when diary-buff
        (set-buffer diary-buff)
        (save-buffer)
        (kill-buffer diary-buff)
        (setq diary-buff (find-file-noselect diary-file)))
      (other-window 1)
      (switch-to-buffer diary-buff)
      (beginning-of-buffer)
      (dolist (entry new-entries)
        (if (listp (car entry))
            (insert (format "%s/%s/%s\n%s\n" 
                            (if (car (car entry)) (format "%d" (car (car entry))) "*")
                            (if (cadr (car entry)) (format "%d" (cadr (car entry))) "*")
                            (if (caddr (car entry)) (format "%d" (caddr (car entry))) "*")
                            (cadr entry)))
          (insert (format "%s\n%s\n" (car entry) (cadr entry)))))
      (beginning-of-buffer)
      (when (buffer-modified-p)
        (if (y-or-n-p "This would be your new diary.  Save? ")
            (save-buffer)
          (kill-buffer diary-buff)))
      (other-window 1)
      (when (fboundp 'appt-initialize)
        (appt-initialize))
      )))

;;;###autoload
(defun diary-pilot-to-pilot ()
  "Push the diary out to the pilot and get the new pilot appointments into the diary."
  (interactive)
  (message "Selecting records...")
    
  (save-excursion (diary-pilot-to-pilot-file diary-pilot-to-pilot-file))
  (let ((tz (getenv "TZ")))
    (save-excursion
      (setenv "TZ" "GMT")
      (pilot-run-command (format "install-datebook -p %s -r %s" pilot-device diary-pilot-to-pilot-file)
                         "Press the HotSync button now to export diary.")
      (pilot-run-command (format "pilot-dedupe -p %s %s" pilot-device diary-pilot-database)
                         "Press the HotSync button again to remove duplicates.")
      (diary-pilot-insert-pilot-entries)
      (message "All done.")
      (setenv "TZ" tz))))

;; diary-pilot.el ends here
