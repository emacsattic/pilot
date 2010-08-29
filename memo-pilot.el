;;; memo-pilot.el --- imports pilot memos into a mailbox, and exports
;;;                   messages and buffers as pilot memos.

;; Copyright (C) 2005 Stefan D. Bruda <bruda@cs.ubishops.ca>

(defvar memo-pilot-version "1.2"
  "memo-pilot version number"
)

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
;; This is part of the `emacs-pilot' package
;; <http://turing.ubishops.ca/home/bruda/emacs-pilot/>.  For detailed
;; usage information please refer to the README file included in the
;; package or accessible directly at
;; <http://turing.ubishops.ca/home/bruda/emacs-pilot/emacs-pilot/README>
;;
;; This code is intended to be used from within the pilot package and
;; is from within the `pilot.el' file (which see).  You can use it
;; however in a stand-alone manner (case in which you also need the
;; mentioned `pilot.el' and the `pilot-wait' and `pilot-wait-end'
;; shell scripts).  The function imports pilot memos is
;; `pilot-get-memos', the function that writes a VM message as memo is
;; `vm-message-to-pilot', and the function that writes the current
;; buffer as memo is `save-buffer-to-pilot' (the latter function is
;; gotten from Andrew J Cosgriff's `pilot-memo.el').

;;; History:

;; Version 1.2:
;;  o  FSF Emacs compatibility fixes.
;;  o  Augmented documentation.
;; Version 1.1:
;;  o  Workaround for emacsen lacking `temp-directory'.
;; Version 1.0 released.

;;; Code:

(provide 'memo-pilot)
(require 'pilot)

(defcustom memo-pilot-mailbox "~/Mail/pilot"
  "*Mailbox for the memos retrieved from the pilot."
  :type 'string
  :group 'pilot)

(defcustom memo-pilot-temp-file (expand-file-name 
                                 "memo-to-pilot" 
                                 (if (fboundp 'temp-directory) (temp-directory) "/tmp"))
  "*Temporary file for the transfer of emails to the pilot."
  :type 'string
  :group 'pilot)

(defcustom memo-pilot-category "Unfiled"
  "Default category to upload memos into."
  :type 'string
  :group 'pilot)

(defvar memo-pilot-title-history nil)
(defvar memo-pilot-category-history (list memo-pilot-category))

(defun memo-pilot-memos-to-mbox (file)
  "Converts the memo file as returned by the pilot-link's `memos' into a real mbox, 
redabale by email clients such as VM.  Will not alter the mailbox if
the format is already correct."
  (let ((date (replace-in-string (with-output-to-string
                                   (shell-command "date" t)) "\n" ""))
        last)
    (set-buffer (find-file-noselect file))
    (beginning-of-buffer)
    (setq last (point))
    (while (search-forward-regexp "^From: " nil t)
      (unless (save-excursion
                (search-backward-regexp "^From " last t))
        (beginning-of-line)
        (insert (format "\nFrom pilot %s\n" date))
        (search-forward-regexp "^From: " nil t))
      (setq last (point)))
    (beginning-of-buffer)
    (while (equal "\n" (buffer-substring (point) (+ (point) 1)))
      (delete-region (point) (+ (point) 1)))
    (save-buffer)
    (kill-buffer nil)
    ))

(defun memo-pilot-message-to-memo (&optional continue-p)
  "Gets a mail message from the current buffer and transfers it as a memo to the pilot.
If CONTINUE-P is not nil then processes the next message after point;
otherwise processes the first message in the current buffer."
  (let (category subject message here there)
    (save-excursion
      (unless continue-p
        (beginning-of-buffer))
      (if (search-forward-regexp "^Subject: \\[\\(.*\\)\\] \\(.*\\)$" nil t)
          (setq category (buffer-substring (match-beginning 1) (match-end 1))
                subject (buffer-substring (match-beginning 2) (match-end 2)))
        (search-forward-regexp "^Subject: \\(.*\\)$" nil t)
        (setq category memo-pilot-category
              subject (buffer-substring (match-beginning 1) (match-end 1))))
      (if (search-forward "\n\n" nil t)
          (progn
            (setq here (point)
                  there (if (search-forward-regexp "^From" nil t) (- (point) (length "From")) (point-max))
                  message (replace-in-string (buffer-substring here there)
                                             (concat subject "\n") "")
                  )
            (goto-char there))
        (search-forward "^From" nil t)
        (goto-char (- (point) (length "From")))
        (setq message ""))
      ;;(list category subject message)
      (set-buffer (find-file-noselect memo-pilot-temp-file))
      (erase-buffer)
      (insert (concat subject "\n" message))
      (save-buffer)
      (kill-buffer nil)
      (pilot-run-command (format "install-memo -p %s -c %s %s" 
                                 pilot-device category memo-pilot-temp-file)
                     (format "Press the HotSync button now to export memo [%s] %s." category subject))
      )))

;;;###autoload
(defun save-buffer-to-pilot (subject category)
 "Save the current buffer as a memo to the pilot.
SUBJECT is the memo title to be used on the pilot side, which defaults
to the buffer name.  CATEGORY is the memo category to place the memo
in on the pilot side."
  (interactive
   (list
    (read-from-minibuffer
     "Save to pilot with title: " (buffer-name) nil nil 'memo-pilot-title-history)
    (read-from-minibuffer
     "Save to pilot under category: " (nth 0 memo-pilot-category-history) nil nil
     (cons 'memo-pilot-category-history 1))))
  (let ((pilot-memo-buffer (find-file-noselect memo-pilot-temp-file)))
    (save-excursion
      (copy-region-as-kill (point-min) (point-max))
      (set-buffer pilot-memo-buffer)
      (erase-buffer)
      (insert subject "\n")
      (yank)
      (delete-windows-on pilot-memo-buffer)
      (save-buffer)
      (kill-buffer pilot-memo-buffer)
      (pilot-run-command (format "install-memo -p %s -c %s %s" 
                                 pilot-device category memo-pilot-temp-file)
                         (format "Press the HotSync button now to export memo [%s] %s."
                                 category subject)))))

;;;###autoload
(defun pilot-get-memos ()
  "Get the memos from the pilot and puts them into the `memo-pilot-mailbox' mail box.
There is no check to see whether the memo has already been imported,
so the mailbox may contain duplicates as a result of this operation."
  (interactive)
  (save-excursion
    (let ((buf (set-buffer (find-file-noselect memo-pilot-mailbox))))
      (if (and (featurep 'vm) (memq major-mode '(vm-mode vm-virtual-mode)))
          (vm-quit)
        (save-buffer)
        (kill-buffer buf))))
  (if pilot-running-xemacs 
      (progn (redraw-frame nil t)
             (redraw-modeline t))
    (redraw-display))
  (pilot-run-command (format "memos -p %s >> %s" pilot-device memo-pilot-mailbox)
                     "Press the HotSync button now to import memos.")
  (memo-pilot-memos-to-mbox memo-pilot-mailbox)
  (message "Done.")
  )

;;;###autoload
(defun vm-message-to-pilot ()
  "Exports the currently displayed message as memo to the pilot."
  (interactive)
  ;; stolen from vm-page.el
  (vm-select-folder-buffer)
  (vm-check-for-killed-summary)
  (vm-check-for-killed-presentation)
  (vm-error-if-folder-empty)
  (memo-pilot-message-to-memo))

;; memo-pilot.el ends
