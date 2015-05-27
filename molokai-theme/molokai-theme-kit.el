;;; molokai-theme-kit.el

;; Copyright (C) 2013 Huang Bin

;; Author: Huang Bin <embrace.hbin@gmail.com>
;; URL: https://github.com/hbin/molokai-theme
;; Version: 0.8

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is optional UI settings for molokai theme.
;;
;; Include: limun style, mode line...

;;; Requirements:
;;
;; Emacs 24

;;; Code:

;;
;; linum style
;;
(require 'linum)
(setq linum-format "%4d ")

(toggle-indicate-empty-lines nil)

;;
;; mode-line
;;
;; http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
(setq-default mode-line-format
              (list
               ;; line and column
               " "
               (propertize "%c" 'face 'font-lock-type-face)
               " "
               (propertize "%l" 'face 'font-lock-type-face)
               " "
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize "%b" 'face 'font-lock-keyword-face 'help-echo (buffer-file-name)))               
               ;; was this buffer modified since the last save?
               '(:eval (when (buffer-modified-p)
                         (concat " "
                                 (propertize "modified"
                                             'face 'font-lock-preprocessor-face
                                             'help-echo "Buffer has been modified"))))
               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat " "
                                 (propertize "read-only"
                                             'face 'font-lock-preprocessor-face
                                             'help-echo "Buffer is read-only"))))
               " "

               ;; the current major mode for the buffer.
               '(:eval (propertize "%m" 'face 'font-lock-string-face 'help-echo buffer-file-coding-system))
               " "
               
               ;; relative position, size of file
               (propertize "%I" 'face 'font-lock-constant-face)
               " "
               (propertize "%p" 'face 'font-lock-constant-face)

               ;; "[" ;; insert vs overwrite mode, input-method in a tooltip
               ;; '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
               ;;                     'face 'font-lock-preprocessor-face
               ;;                     'help-echo (concat "Buffer is in "
               ;;                                        (if overwrite-mode "overwrite" "insert") " mode")))               

               ;; add the time, with the date and the emacs uptime in the tooltip
               ;; '(:eval (propertize (format-time-string "%H:%M")
               ;;                     'help-echo
               ;;                     (concat (format-time-string "%c; ")
               ;;                             (emacs-uptime "Uptime:%hh"))))

               ;; " --"
               ;; i don't want to see minor-modes; but if you want, uncomment this:
               ;; minor-mode-alist  ;; list of minor modes
               ;; "%-" ;; fill with '-'
               ))

(provide 'molokai-theme-kit)
