multishell.el
=============

Organize multiple local and remote Emacs shell buffers.

Multishell is available via Emacs package manager, [in ELPA](https://elpa.gnu.org/packages/multishell.html). Install "multishell" from the `M-x package-list-packages` listing.

Sometimes it seems like I have as many emacs shell buffers going as I do
tabs in my browser: one for each project I'm currently working on, one for
the remote host I'm interacting with and sudo shells (and remote sudo
shells), one for doing maintenance chores that go across projects, and so
on. Multishell makes it easy to organize my use of them all with distinct
names I assign to each one, tramp syntax to specify remote and / or sudo
shells, a roster of them all that can be saved between emacs sessions
(preserving remote and sudo specifications), and a single key binding to
access within and manage the collection.

With a customizable master keybinding, multishell makes it easy to:

* Get to the input point from wherever you are in a shell buffer,
  or to any of your shell buffers, from anywhere inside emacs.

* Use universal arguments and name completion to launch a new or choose
  among existing shell buffers, and change which is the current default.

* Easily restart exited shells, or shells from prior emacs sessions.

* Specify an initial path for the shell. By using Emacs tramp syntax you
  can launch a sudo and/or remote shell.

  For example, specifying the following at the multishell buffer name
  prompt to:

  * `#root/sudo:root@localhost:/etc` - launch a shell in a buffer named
    "*#root*" with a root shell starting in /etc.

  * `/ssh:example.net:` - launch a shell buffer in your homedir on
    example.net.  The buffer will be named "*example.net*".

  * `#ex/ssh:example.net|sudo:root@example.net:/etc` - launch a root
    shell starting in /etc on example.net named "*#ex*".

  * `interior/ssh:gateway.corp.com|ssh:interior.corp.com:` via
    gateway.corp.com, launch a shell in your homedir on interior.corp.com.
    The buffer will be named "*interior*". You can interject more host
    hops and so on.

* Thanks to tramp, file visits initiated in remote shell buffers will
  seamlessly be on the hosts where the shells are running, in the auspices
  of the account being used.

* Manage your list of shells, current and past, as a collection.

See the `multishell-pop-to-shell` docstring (in
[multishell.el](multishell.el)) for details, and
[getting-to-a-shell.md](getting-to-a-shell.md) for the nitty-gritty
decision tree that determines where the keybinding goes according to the
various conditions.

Customize-group `multishell' to select and activate a keybinding and set
various behaviors. Customize-group `savehist' to preserve buffer
names/paths across emacs restarts.

Please use
[the multishell repository](https://github.com/kenmanheimer/EmacsMultishell)
issue tracker to report problems, suggestions, etc.

See the [multishell.el](multishell.el) file commentary for a change log and
Todo list.
