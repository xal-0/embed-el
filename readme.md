# embed.el

Emacs utilities for programming microcontrollers using OpenOCD.

## Quick setup

Suggested installation uses `quelpa` and `quelpa-use-package`.
Suggested keybindings and their respective commands:

| Key | Command | Description |
| --- | --- | --- |
| `C-z e o` | `embed-openocd-start` | Starts the OpenOCD process. |
| `C-z e O` | `embed-openocd-stop` | Stops the OpenOCD process. |
| `C-z e g` | `embed-openocd-gdb` | Starts GDB and loads the binary onto the micro. |
| `C-z e f` | `embed-openocd-flash` | Flashes the binary onto the micro. |

```elisp
(use-package embed
  :quelpa (embed :fetcher github :repo "sjsch/embed-el")
  :bind (("C-z e o" . embed-openocd-start)
         ("C-z e O" . embed-openocd-stop)
         ("C-z e g" . embed-openocd-gdb)
         ("C-z e f" . embed-openocd-flash)))
```
