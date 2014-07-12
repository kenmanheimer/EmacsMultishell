EmacsUtils
==========

Handy Emacs utilities

I've been using Emacs since it was publicly available (1985 or 1986), and have contributed some items which are included with Emacs, notably the [Allout outliner](http://myriadicity.net/software-and-systems/craft/emacs-allout), [icomplete mode](http://www.emacswiki.org/emacs/IcompleteMode). And python-mode's [pdbtrack functionality](http://myriadicity.net/software-and-systems/craft/crafty-hacks#section-1). Like many long-time Emacs users, I've got some personal custom code, some of which I wouldn't do without. Here's some - I hope to include more that I think would be useful to others, as time allows.

* **pdbtrack.el**
  * Add sensitivity to comint shells so the source file lines are automatically
    presented in a separate window when the Python PDB debugger steps to them.

    This is derived from the pdb tracking code, which I originally wrote, and
    which has been included in (various) official Emacs Python modes. I wanted
    a version that I could more easily tweak and maintain, independently of
    the python-mode code.

    It would eventually be nice to generalize this code, to work for things
    like the node.js debugger. We'll see if I (or anyone) ever gets around to
    that.


* **poptoshell.el**
  * I use the emacs shell a lot. This code enables me to streamline and
    extend how I can a single one, or multiple ones in a project-oriented
    fashion:

    * It simplifies getting to the input prompt, by doing the right thing when
      I hit the key I have bound to pop-to-shell (I use [M-space], ie
      meta-space:
    * If the cursor is in a buffer that has no subprocess, pop the window to
      the primary shell buffer
    * If there is no shell buffer, start one.
    * If the cursor is in a buffer which has a process, move the cursor to
      the process input point.
    * With a universal argument, even if the current buffer has a subprocess,
      solicit the name of the target shell buffer - defaulting to the current
      main one - and pop to that.
      * This enables starting an alternate shell buffer, for instance, and/or
        switching between the main and alternate ones.
      * (The expected name is without the surrounding asterisks, and
        completion is done against existing shell buffer names stripped of
        their asterisks.)
      * With a doubled universal arg, prompt for the target shell buffer and
        set the provided name as the primary shell buffer.
    The last few things enable a kind of project-focus mode.  I often have
    various shell buffers, each one associated with a project. As I switch
    which project is currently my primary focus, I use the double universal
    argument to switch which shell buffer is the default. I can still use the
    single universal argument to easily switch to any of the shells, but most
    easily to my current primary.
