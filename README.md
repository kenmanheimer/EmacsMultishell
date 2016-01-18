multishell.el
=============

Facilitate interaction with multiple local and remote Emacs shell buffers.

I use the emacs shell a *lot*. On top of emacs' powerful shell and tramp
facilities, use a `multishell` (customization-activated) key binding to:

* Get to the input point from wherever you are in a shell buffer,
* ... or to one of your shell buffers if you're not currently in one.
* Use universal arguments to launch and choose among alternate shell buffers,
* ... and select which is default.
* Append a path to a new shell name to launch a shell in that directory,
* ... and use a path with Emacs tramp syntax to launch a remote shell.

  For example: 

  * `/ssh:example.net:/` for a shell buffer in / on
    example.net; the buffer will be named "*example.net*".

  * `#ex/ssh:example.net|sudo:root@example.net:/etc` for a root shell
    starting in /etc on example.net named "*#ex*".

(NOTE that there is a frequent problem with specifying a remote homedir
using tramp syntax, eg `/ssh:example.net:` or `/ssh:example.net:~`. That
sometimes fails on an obscure bug - particularly for remote+sudo with
homedir syntax. Until fixed, you may need to start remote+sudo shells with
an explicit path, then cd ~. With `multishell`s dir-tracking persistent history, you'll be able to use completion to start that shell in the right place, in your subsequent sessions.)

Customize-group `multishell` to select and activate a keybinding and set
various behaviors. Customize-group `savehist` to preserve buffer
names/paths across emacs sessions.

See the `multishell-pop-to-shell` docstring for details.
