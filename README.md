multishell.el
=============

Facilitate use of multiple local and remote Emacs shell buffers.

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

Change Log
----------
* 2016-01-16 1.0.5 Ken Manheimer:
  - History now includes paths, when designated
  - Actively track current directory in history entries that have a path.
    Custom control: multishell-history-entry-tracks-current-directory
  - Offer to remove shell's history entry when buffer is killed
    (For now, the only UI way to remove history entries.)
  - Fix - prevent duplicate entries for same name but different paths
  - Fix - recognize and respect tramp path syntax to start in home dir
    - But tramp bug, remote+sudo hops to a home dir can fail, get wedged.
  - Simplify history var name, migrate existing history if any from old name
* 2016-01-04 Ken Manheimer - Released to ELPA
* 2016-01-02 Ken Manheimer - working on this in public, but not yet released.

 TODO
----------
* Isolate tramp sporadic failure to connect to remote+sudo+homedir syntax
  (eg, /ssh:xyz.com|sudo:root@xyz.com: or /ssh:xyz.com|sudo:root@xyz.com:~)
* Find suitable, internally consistent ways to sort tidy completions, eg:
  - first list completions for active shells, then present but inactive,
    then historical
  - some way for user to toggle between presenting just buffer names vs
    full buffer/path
    - without cutting user off from easy editing of path
* Find proper method for setting field boundary at beginning of tramp path
  in the minibuffer, in order to see whether the field boundary magically
  enables tramp completion of the path.
* Assess whether option to delete history entry on kill-buffer is
  sufficient.
