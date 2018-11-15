;;; embed.el --- Utilities for developing embedded software with OpenOCD  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Sam Schweigel

;; Author:  Sam Schweigel <s.schweigel@gmail.com>
;; Version: 0.2.0
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
(defvar embed-gdb-command "arm-none-eabi-gdb"
  "The default command to start GDB with.")

(defvar embed/openocd-process nil "The running openocd instance.")

(defun embed/find-openocd-cfg ()
  (f-traverse-upwards
   (lambda (path)
     (f-exists? (f-expand embed-openocd-cfg-name path)))
   nil))

;;;###autoload
(defun embed-openocd-start ()
  "Start OpenOCD, traversing the directory tree upwards to find a
configuration file named `embed-openocd-cfg-name'.  Stop OpenOCD
with \\[embed-openocd-stop]."
  (interactive)
  (if (and embed/openocd-process
	   (eq (process-status embed/openocd-process) 'run))
      (progn
	(message "OpenOCD is already running.")
	nil)
    (let* ((dir (embed/find-openocd-cfg))
	   (cfg (f-expand embed-openocd-cfg-name (embed/find-openocd-cfg)))
	   (args (format "-f%s" cfg)))
      (if dir
	  (setq embed/openocd-process
		(start-process "openocd" "*openocd*" embed-openocd-process-name args))
	(progn
	  (message "Could not find OpenOCD configuration file %s"
		   embed-openocd-cfg-name)
	  nil)))))

;;;###autoload
(defun embed-openocd-stop ()
  "Stop OpenOCD if it is running.  Start OpenOCD with
\\[embed-openocd-start]."
  (interactive)
  (if (and embed/openocd-process
	   (eq (process-status embed/openocd-process) 'run))
      (delete-process embed/openocd-process)
    (progn
      (message "OpenOCD is not running.")
      nil)))

;;;###autoload
(defun embed-openocd-gdb ()
  "Start GDB and OpenOCD if necessary, and load the binary onto
the microcontroller."
  (interactive)
  (if (and embed/openocd-process
	   (eq (process-status embed/openocd-process) 'run))
      (let ((flash (read-file-name "flash: ")))
	(gdb (format "%s -i=mi %s" embed-gdb-command flash))
	(sit-for 1)
	(gud-basic-call "target remote localhost:3333")
	(gud-basic-call "monitor reset halt")
	(gud-basic-call "load"))
    (when (embed-openocd-start)
      (embed-openocd-gdb))))

(provide 'embed)
;;; embed.el ends here
