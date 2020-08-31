;;; ios-config-mode.el --- Edit Cisco IOS configuration files

;; Copyright (C) 2004 Noufal Ibrahim <nkv at nibrahim.net.in>
;;
;; Author: Noufal Ibrahim <nkv at nibrahim.net.in>
;; Maintainer: Matthew Sojourner Newton <matt@mnewton.com>
;; Created: 9 Nov 2010
;; Homepage: https://github.com/mnewt/IOS-config-mode
;; Keywords: extensions
;; Package-Requires: ((emacs "24.3"))
;; License: GPL-2.0
;; Version: 0.5
;;
;; This program is not part of Gnu Emacs
;;
;; ios-config-mode.el is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Commentary:

;; ios-config-mode is an Emacs major mode that helps to view and edit Cisco IOS
;; (tm) configuration files.
;;
;; See the README for more information.

;;; Code:

(defgroup ios-config nil
  "View and edit Cisco IOS (tm) configuration files."
  :group 'languages
  :prefix "ios-config-")

(defvar ios-config-mode-hook nil
  "Hook called by `ios-config-mode'.")

(defvar ios-config-mode-map
  (let ((ios-config-mode-map (make-keymap)))
    (define-key ios-config-mode-map "\C-j" 'newline-and-indent)
    ios-config-mode-map)
  "Keymap for Cisco router configuration major mode.")

(defface ios-config-ipadd-face
  '((t (:inherit font-lock-type-face)))
  "Face for IP addresses.")

(defface ios-config-command-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for basic router commands.")

(defface ios-config-toplevel-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for top level router commands.")

(defface ios-config-no-face
  '((t (:underline t)))
  "Face for \"no\" prefixed commands.")

(defconst ios-config-font-lock-keywords
  `((,(concat "\\_<"
       (regexp-opt '("access-list" "class-map" "controller" "interface" "vrf"
                     "line" "policy-map" "redundancy-map" "route-map"
                     "object-group" "access-group" "cluster" "username"
                     "service-policy") t)
       "\\_>")
     . 'ios-config-toplevel-face)
    (,(concat "\\_<"
       (regexp-opt '("alias" "boot" "card" "diagnostic" "enable" "hostname"
                     "logging" "service" "snmp-server" "version" "vtp" "names"
                     "description" "lacp" "port-channel" "mac-address" "vlan"
                     "nameif" "security-level" "ip" "ospf" "ftp"
                     "network-object" "service-object" "icmp-object"
                     "protocol-object" "group-object" "host" "network" "mtu"
                     "icmp" "asdm" "prefix-list" "timeout" "user-identity" "aaa"
                     "http" "crypto" "telnet" "ssh" "console" "threat-detection"
                     "ssl" "service-type" "match" "class" "parameters" "inspect"
                     "prompt" "jumbo-frame" "shutdown" "address"
                     "management-only" "destination" "extended" "permit" "any"
                     "type" "for" "priority" "health-check" "failover"
                     "monitor-interface" "arp" "area" "server" "scopy"
                     "message-length" "call-home" "password" "encrypted"
                     "privilege") t)
       "\\_>")
     . 'ios-config-command-face)
    ("\\<\\(no\\)\\>" . ios-config-no-face)
    ("\\<\\([0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\)\\>"
     . 'ios-config-ipadd-face))

  "Font locking definitions for cisco router mode.")

;; Imenu definitions.
(defvar ios-config-imenu-expression
  '(("Interfaces"        "^[\t ]*interface *\\(.*\\)" 1)
    ("VRFs"              "^ip vrf *\\(.*\\)" 1)
    ("Controllers"       "^[\t ]*controller *\\(.*\\)" 1)
    ("Routing protocols" "^router *\\(.*\\)" 1)
    ("Class maps"        "^class-map *\\(.*\\)" 1)
    ("Policy maps"       "^policy-map *\\(.*\\)" 1)))

;; Indentation definitions.
(defun ios-config-indent-line ()
  "Indent current line as cisco router config line."
  (let ((indent0 "^interface\\|redundancy\\|^line\\|^ip vrf \\|^controller\\|^class-map\\|^policy-map\\|router\\|access-list\\|route-map")
        (indent1 " *main-cpu\\| *class\\W"))
    (beginning-of-line)
    (let ((not-indented t)
          (cur-indent 0))
      (cond ((or (bobp) (looking-at indent0) (looking-at "!")) ; Handles the indent0 and indent1 lines
             (setq not-indented nil
                   cur-indent 0))
            ((looking-at indent1)
             (setq not-indented nil
                   cur-indent 1)))
      (save-excursion ; Indents regular lines depending on the block they're in.
        (while not-indented
          (forward-line -1)
          (cond ((looking-at indent1)
                 (setq cur-indent 2
                       not-indented nil))
                ((looking-at indent0)
                 (setq cur-indent 1
                       not-indented nil))
                ((looking-at "!")
                 (setq cur-indent 0
                       not-indented nil))
                ((bobp)
                 (setq cur-indent 0
                       not-indented nil)))))
      (indent-line-to cur-indent))))

(defvar ios-config-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table) ;All _'s are part of words.
    (modify-syntax-entry ?- "w" table) ;All -'s are part of words.
    (modify-syntax-entry ?! "<" table) ;All !'s start comments.
    (modify-syntax-entry ?\n ">" table) ;All newlines end comments.
    (modify-syntax-entry ?\r ">" table)) ;All linefeeds end comments.
  "Syntax table for cisco router mode.")

;;;###autoload
(define-derived-mode ios-config-mode prog-mode "IOS Configuration"
  "Major mode for editing Cisco IOS (tm) configuration files.

\\{ios-config-mode-map\}"
  :syntax-table ios-config-mode-syntax-table
  :after-hook ios-config-mode-hook
  (setq-local indent-line-function 'ios-config-indent-line
              comment-start "!"
              comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)!+ *"
              imenu-case-fold-search nil
              imenu-generic-expression ios-config-imenu-expression)
  (font-lock-add-keywords 'ios-config-mode ios-config-font-lock-keywords)
  (imenu-add-to-menubar "Imenu"))
              
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cfg\\'" . ios-config-mode))

;;;###autoload
(defun ios-config-add-command-to-interfaces (command &optional intf)
  "Add COMMAND to interfaces that match regexp INTF.

If COMMAND is already there, do not change it.

If INTF is null, work on all interfaces.
   This requires the buffer to be indented properly."
  (interactive "MCommand: \nMInterface: ")
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (progn
               (if (looking-at (concat "^[Ii]nterface " intf))
                   (let ((block-end (ios-config-find-interface-block (point))))
                     (if (not (ios-config-command-in-block-p command block-end))
                         (save-excursion
                           (goto-char (+ block-end 1))
                           (insert " " command "\n")))))
               (not (= (forward-line) 1)))))))
  

(defun ios-config-find-interface-block (pt)
  "Find an interface configuration block around PT."
  (save-excursion
    (goto-char pt)
    (forward-line)
    (while (looking-at "^ .*")
      (forward-line 1))
    (- (point) 1)))
      
(defun ios-config-command-in-block-p (cmd end)
  "Return non-nil if CMD is in a block.

Limit search to END."
  (save-excursion
    (search-forward-regexp cmd end t)))
  
;;;###autoload
(defun ios-config-unshut-all-interfaces ()
  "Remove the \"shutdown\" or \"shut\" command from all interfaces.

Add a \"no shutdown\" instead."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\([ \t]*shut\\(down\\)?\\)$" nil t)
      (beginning-of-line)
      (kill-line 1)))
  (ios-config-add-command-to-interfaces "no shutdown"))

;;;###autoload
(defun ios-config-shut-all-interfaces ()
  "Remove the \"no shutdown\" or \"no shut\" command from all interfaces.

Add a \"shutdown\" instead."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\([ \t]*no shut\\(down\\)?\\)$" nil t)
      (beginning-of-line)
      (kill-line 1)))
  (ios-config-add-command-to-interfaces "shutdown"))

(provide 'ios-config-mode)

;;; ios-config-mode.el ends here
