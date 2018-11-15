;;; embed.el --- Utilities for developing embedded software with OpenOCD  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Sam Schweigel

;; Author:  Sam Schweigel <s.schweigel@gmail.com>
;; Version: 0.1.0
;; Keywords: tools, processes
;; Package-Requires: ((f "0.20.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(defvar embed-openocd-cfg-name "openocd.cfg"
  "The name of the openocd configuration file to search for.")
(defvar embed-openocd-process-name "openocd"
  "The name of the OpenOCD command.")

(defvar embed/openocd-process nil "The running openocd instance.")

(defun embed/find-openocd-cfg ()
  (f-traverse-upwards
   (lambda (path)
     (f-exists? (f-expand embed-openocd-cfg-name path)))
   nil))

;;;###autoload
(defun embed-openocd-start ()
  (interactive)
  (if (and embed/openocd-process
	   (eq (process-status embed/openocd-process) 'run))
      (message "OpenOCD is already running.")
    (let* ((dir (embed/find-openocd-cfg))
	   (cfg (f-expand embed-openocd-cfg-name (embed/find-openocd-cfg)))
	   (args (format "-f%s" cfg)))
      (if dir
	(setq embed/openocd-process
	      (start-process "openocd" "*openocd*" embed-openocd-process-name args))
	(message "Could not find OpenOCD configuration file %s"
		 embed-openocd-cfg-name)))))

;;;###autoload
(defun embed-openocd-stop ()
  (interactive)
  (if (and embed/openocd-process
	   (eq (process-status embed/openocd-process) 'run))
      (delete-process embed/openocd-process)
    (message "OpenOCD is not running.")))

(provide 'embed)
;;; embed.el ends here
