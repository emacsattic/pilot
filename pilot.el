;;; pilot.el --- package for synchronization of various Emacs subsystems with Palm Pilot.

;; Copyright (C) 2005 Stefan D. Bruda <bruda@cs.ubishops.ca>

(defvar pilot-version "1.4"
  "emacs-pilot version number"
)

;; Based on code by Jamie Zawinski <jwz@jwz.org>, Noah Friedman
;; <friedman@splode.com>, Martin Schwenke <martin@meltin.net>.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Various functions transfer the following databases to and from the
;; pilot: BBDB, diary, and memos.  This is part of the `emacs-pilot'
;; package <http://turing.ubishops.ca/home/bruda/emacs-pilot/>.  For
;; detailed usage information please refer to the README file included
;; in the package or accessible directly at
;; <http://turing.ubishops.ca/home/bruda/emacs-pilot/emacs-pilot/README>
;;
;; To use the package, insert the following line into your Emacs
;; initialization file:
;;
;; (require 'pilot)
;;
;; Autoloads are also available if preferred for the following
;; functions (the first two being the main functions).
;;   pilot-make-menu (creates the pilot menu, from pilot.el)
;;   pilot-add-to-vm (integrates the pilot menu into VM, from pilot.el)
;;   bbdb-pilot-to-pilot (pushes the BBDB database to the pilot, from
;;                        bbdb-pilot.el)
;;   bbdb-pilot-to-bbdb (integrates the pilot address book database into 
;;                       BBDB, from bbdb-pilot.el)
;;   diary-pilot-to-pilot (synchronizes the diary, from diary-pilot.el)
;;   pilot-get-memos (puts pilot memos into a mailbox, from memo-pilot.el)
;;   vm-message-to-pilot (exports the current message as memo, from memo-pilot.el)
;;
;; The package creates the `pilot' customization group, see the
;; variables therein for customization.
;;
;; You need the `pilot-wait' and `pilot-wait-end' scripts or
;; equivalent (or set the appropriate customizable variables to the
;; empty string) included in this distribution.  You also need the
;; `pilot-link' package which does the actual transfer to and from the
;; pilot.
;;
;; Actually, the two scripts are needed only for pure udev systems,
;; more specifically on those systems in which /dev/pilot appears when
;; the hotsync button is pressed and disappears when the hotsync
;; operation completes.  On system in which /dev/pilot persists
;; between hotsyncs `pilot-wait' has no effect (so you are better off
;; setting `pilot-wait-command' to the empty string) and the use of
;; `pilot-wait-end' is harmful (so you MUST set `pilot-end-command' to
;; the empty string, otherwise your Emacs will hang after the first
;; hotsync operation!).

;;; History:

;; Version 1.4:
;;  o  Fixes in bbdb-pilot.el and diary-pilot.el
;;  o  Loading one component (i.e., requiring one feature) no longer
;;     loads all the others but only the code necessary for that
;;     component to work.
;; Version 1.3:
;;  o  More compatibility fixes.
;; Version 1.2:
;;  o  FSF Emacs compatibility fixes.
;;  o  Augmented documentation.
;; Version 1.1:
;;  o  Sanitized namespaces (some custom variable names changed!)
;;  o  Workaround for emacsen lacking `temp-directory'
;; Version 1.0 released.

;;; Code:

(defgroup pilot nil
  "Palm pilot support")

(defcustom pilot-wait-command "pilot-wait"
  "*String to prefix all the pilot-related commands, 
meant to wait until /dev/pilot is ready on udev systems.  If your
/dev/pilot is always there then you are better off setting this to the
empty string."
  :type 'string
  :group 'pilot)

(defcustom pilot-end-command "pilot-wait-end"
  "*Command to be run after any pilot-related commands, 
meant to wait for /dev/pilot to disappear (so that a new command can
be issued) on udev systems.  If your /dev/pilot does not disappear
after the completion of a hotsync operation then you *must* set this
to the empty string otherwise your Emacs will hang after the first
hotsync."
  :type 'string
  :group 'pilot)

(defcustom pilot-device "/dev/pilot"
  "*Pilot device."
  :type 'string
  :group 'pilot)

(require 'easymenu)

(unless (featurep '(or bbdb-pilot diary-pilot memo-pilot))
  ;; we load the other components only when called top-level
  (require 'bbdb-pilot)
  (require 'diary-pilot)
  (require 'memo-pilot))

;;;###autoload
(defun pilot-make-menu ()
  "Shows the Pilot menu."
  (interactive)
  (easy-menu-define 
   pilot-main-menu nil
   "Menu for the pilot interaction."
   (list
    "Pilot"
    ["Message to memo" vm-message-to-pilot (featurep 'vm)]
    ["Get memos" pilot-get-memos  (featurep 'vm)]
    ["Diary sync" diary-pilot-to-pilot (featurep 'appt)]
    ["BBDB to pilot" bbdb-pilot-to-pilot (featurep 'bbdb)]
    ["Pilot to BBDB" bbdb-pilot-to-bbdb (featurep 'bbdb)]
    ))

  (easy-menu-add pilot-main-menu))

;;;###autoload
(defun pilot-add-to-vm ()
  "Adds the pilot menu to the relevant VM buffers."
  (add-hook 'vm-mode-hook 'pilot-make-menu)
  (add-hook 'vm-summary-mode-hook 'pilot-make-menu)
  (add-hook 'vm-virtual-mode-hook 'pilot-make-menu))

(defun pilot-run-command (command &optional message insert)
  "Runs COMMAND prefixed by `pilot-wait-command', followed by
`pilot-end-command', and with appropriate feedback to the user
consisting in the optional MESSGAE if provided and a generic prompt to
press the hotsync button otherwise. Intended to be used for all the
pilot-related commands.  If INSERT is non-nil then the result is
inserted in the current buffer, otherwise the output is discarded."
  (unless message (setq message "Press the HotSync button now."))
  (unless insert
    (when (get-buffer "**pilot-shell**")
      (kill-buffer "**pilot-shell**"))
    (generate-new-buffer "**pilot-shell**")
    (set-buffer "**pilot-shell**")
    (end-of-buffer))
  (message message)
  (insert (concat "\n*** " command "\n"))
  (shell-command (concat pilot-wait-command " " command) t)
  (message "Cleaning up...")
  (insert "\n*** pilot-wait-end\n")
  (unless (equal pilot-end-command "")
    (shell-command pilot-end-command t))
  (unless insert
    (when (get-buffer "**pilot-shell**")
      (kill-buffer "**pilot-shell**")))
  (message "Cleaning up... done."))

(defun pilot-debug-log (msg)
  "Logs MSG (a string) into **pilot-debug** (debugging purpose only)."
  (save-excursion
    (unless (get-buffer "**pilot-debug**")
      (generate-new-buffer "**pilot-debug**"))
    (set-buffer "**pilot-debug**")
    (insert msg)
    (insert "\n")
    t))

(defvar pilot-running-xemacs 
  (string-match "XEmacs\\|Lucid" emacs-version))

(unless (fboundp 'subst-char-in-string)
  (defun subst-char-in-string (fromchar tochar string &optional inplace)
    "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
    (let ((i (length string))
          (newstr (if inplace string (copy-sequence string))))
      (while (> i 0)
        (setq i (1- i))
        (if (eq (aref newstr i) fromchar)
            (aset newstr i tochar)))
      newstr)))

;; Compatibility stuff (some Emacsen do not provide
;; `replace-regexp-in-string')

(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp rep string &optional
                                          fixedcase literal subexp start)
    "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

    ;; To avoid excessive consing from multiple matches in long strings,
    ;; don't just call `replace-match' continually.  Walk down the
    ;; string looking for matches of REGEXP and building up a (reversed)
    ;; list MATCHES.  This comprises segments of STRING which weren't
    ;; matched interspersed with replacements for segments that were.
    ;; [For a `large' number of replacments it's more efficient to
    ;; operate in a temporary buffer; we can't tell from the function's
    ;; args whether to choose the buffer-based implementation, though it
    ;; might be reasonable to do so for long enough STRING.]
    (let ((l (length string))
          (start (or start 0))
          matches str mb me)
      (save-match-data
        (while (and (< start l) (string-match regexp string start))
          (setq mb (match-beginning 0)
                me (match-end 0))
          ;; If we matched the empty string, make sure we advance by one char
          (when (= me mb) (setq me (min l (1+ mb))))
          ;; Generate a replacement for the matched substring.
          ;; Operate only on the substring to minimize string consing.
          ;; Set up match data for the substring for replacement;
          ;; presumably this is likely to be faster than munging the
          ;; match data directly in Lisp.
          (string-match regexp (setq str (substring string mb me)))
          (setq matches
                (cons (replace-match (if (stringp rep)
                                         rep
                                       (funcall rep (match-string 0 str)))
                                     fixedcase literal str subexp)
                      (cons (substring string start mb) ; unmatched prefix
                            matches)))
          (setq start me))
        ;; Reconstruct a string from the pieces.
        (setq matches (cons (substring string start l) matches)) ; leftover
        (apply #'concat (nreverse matches))))))

(unless (fboundp 'replace-in-string)
  (defun replace-in-string (str regexp newtext &optional literal)
    "Replace all matches in STR for REGEXP with NEWTEXT string,
 and returns the new string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat `\\' in NEWTEXT as special:
  `\\&' in NEWTEXT means substitute original matched text.
  `\\N' means substitute what matched the Nth `\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  `\\\\' means insert one `\\'.
  `\\u' means upcase the next character.
  `\\l' means downcase the next character.
  `\\U' means begin upcasing all following characters.
  `\\L' means begin downcasing all following characters.
  `\\E' means terminate the effect of any `\\U' or `\\L'."
    (if (> (length str) 50)
        (let ((cfs case-fold-search))
          (with-temp-buffer
            (setq case-fold-search cfs)
            (insert str)
            (goto-char 1)
            (while (re-search-forward regexp nil t)
              (replace-match newtext t literal))
            (buffer-string)))
      (let ((start 0) newstr)
        (while (string-match regexp str start)
          (setq newstr (replace-match newtext t literal str)
                start (+ (match-end 0) (- (length newstr) (length str)))
                str newstr))
        str))))

(provide 'pilot)

;; pilot.el end
