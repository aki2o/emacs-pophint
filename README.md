What's this?
============

This is a extension of Emacs that provide navigation like the Vimperator Hint Mode of Firfox.

Do you know Vimperator of Firefox Addon?  
If you don't know it, see a screenshot below.

![vimperator](image/vimperator.png)

The hints that has a alphabet code are shown when you push a "f" key on Firefox like above.  
Then, if you push the alphabet, you can jump the linked URL.

This extension provides same interface on Emacs.

Screenshot
==========

Show the simplest using way.  
If some buffer is opened like the following.

![demo1](image/demo1.png)

If you execute the command that are provided by this extension, the buffer is like the following.

![demo2](image/demo2.png)

If you push the shown alphabet, cursol is moved to the point like the following.  
The following is in the case that pushed "yj". Large alphabet is shown, but inputing small alphabet is OK.

![demo3](image/demo3.png)

Feature
=======

### Scalability

This extension provides the following interface.

1. show the pop-up hints *SOMEWHERE* on any window.
2. do *ACTION* to the point and text that is selected by user.

You can add or change the *SOMEWHERE* and *ACTION* by definition *SOURCE*.  
*SOURCE* is Alist which has structure like `anything-source-...` of anything.el or `ac-source-...` of auto-complete.el.

And,
if you have a idea about *ACTION* that are avilable for any *SOURCE*,
you can define the *ACTION* for using anytime particularly.  
For example, I think moving cursol to the point and copying the text are fit into the *ACTION*.

Some sources and actions are defined already in pophint-config.el.  
For detail, see pophint-config.el section below.

### Switch source quickly

If you execute the command that handle multiple source, the message is shown in minibuffer like the following.

![switch_source](image/switch_source.png)

Then, you can switch a used source by pushing a "s" key.  
And, you can customize the key binding.

### Show the pop-up hints on other window

If you execute the command that show the pop-up hint in the situation that a frame has some windows,
the message is shown in minibuffer like the following.

![switch_window](image/switch_window.png)

Then, you can switch a target window by pushing a "w" key.  
And, you can customize the key binding.

pophint-config.el
=================

This elisp file define some sources and actions that I want.  
I guess you can define source and action that you want by seeing this elisp code.  
List the contents of this elisp at the following.

### pophint:global-sources

`pophint:global-sources` is a variable. It's a list of source. It's avilable for all buffers.

* Sym ... Characters of symbol.
* Quoted ... Enclosed part by character for quoting. (e.g. ")
* URL/Path ... String like URL or Filepath format.
* Cmt ... Comment part of one line.
* Line ... Text of one line.

### pophint:sources

`pophint:sources` is a buffer local variable. It's a list of source.

* Link ... Formatted text for linking. It's enabled on w3m-mode, help-mode and info-mode.
* Node ... Name part of directory and file in the directory on dired-mode.

### Action for any source

* Yank ... Copy the selected text.
* RangeYank ... Copy the range from the selected point to the point that is selected by showing hints again.

About using action for any source, see Install/Configuration section below.

### Show hints when set mark

When you execute `set-mark-command`, show hints for end point of region and copy the selected region.  
You can customize this behavior.

### Show hints when isearch

When you execute `isearch-exit` after `isearch`, by default it's bound to "RET", show hints and move the selected point.  
You can customize this behavior.

Install/Configuration
=====================

I recommend using el-get for installing this extension.  
Downloading manually or using auto-install.el are OK,
but installing each the following dependency is required in this case.

### If use el-get.el

2013/05/01 Now during an application for registration in el-get.  
2013/06/30 But, not yet regist.  

If you set `el-get-sources` in your .emacs or site-start.el file,  
You can available el-get to install this extension.

    (setq el-get-sources
          '(
            (:name log4e
                   :website "https://github.com/aki2o/log4e"
                   :description "provide logging framework for elisp."
                   :type github
                   :pkgname "aki2o/log4e")
            (:name yaxception
                   :website "https://github.com/aki2o/yaxception"
                   :description "provide framework about exception like Java for elisp."
                   :type github
                   :pkgname "aki2o/yaxception")
            (:name pophint
                   :website "https://github.com/aki2o/emacs-pophint"
                   :description "provide navigation like the Vimperator Hint Mode of Firfox."
                   :type github
                   :pkgname "aki2o/emacs-pophint"
                   :depends (popup log4e yaxception))
            ))
    

### If use auto-install.el

    (auto-install-from-url "https://raw.github.com/aki2o/emacs-pophint/master/pophint.el")
    (auto-install-from-url "https://raw.github.com/aki2o/emacs-pophint/master/pophint-config.el")

### Dependency

* popup.el ... bundled with [auto-complete.el](https://github.com/auto-complete/auto-complete)
* [log4e.el](https://github.com/aki2o/log4e)
* [yaxception.el](https://github.com/aki2o/yaxception)

### Command/API

This extension provides the following commands.

* pophint:do ... A base command. Show hints using given source, action and so on.
* pophint:do-flexibly ... Do `pophint:do` using `pophint:global-sources` and `pophint:sources`.
* pophint:do-interactively ... Inquire about action for user and do `pophint:do-flexibly` using the action. 
* pophint:redo ... Do last `pophint:do` again.

This extension provides the following API.

* pophint:defsource ... Define source.
It's defined as variable named `pophint:source-...` and the command named `pophint:do-...` is defined for the source.
* pophint:defaction ... Define action.
You'll be able to select the action and the command named `pophint:do-flexibly-...` is defined for the action.

For other API and customization, see this elisp code or execute `customize-group "pophint"`.

### Configuration

* pophint-config:set-automatically-when-marking
* pophint-config:set-yank-immediately-when-marking
* pophint-config:set-automatically-when-isearch
* pophint-config:set-relayout-when-rangeyank-start

About them, see this elisp code and Configure section below.

### Configure

    (require 'pophint)
    (require 'pophint-config)

    ;; Customize max of popup tip. Default is 200.
    (setq pophint:popup-max-tips 400)

    ;; When set-mark-command, start pop-up hint automatically.
    (pophint-config:set-automatically-when-marking t)
    ;; When you select the shown hint-tip after set-mark-command, do yank immediately.
    (pophint-config:set-yank-immediately-when-marking t)
    ;; When isearch, start pop-up hint automatically after exit isearch.
    (pophint-config:set-automatically-when-isearch t)
    ;; When start searching the end point of RangeYank, layout the window position temporarily.
    (pophint-config:set-relayout-when-rangeyank-start t)

    ;; Key binding
    (define-key global-map (kbd "C-;") 'pophint:do)
    (define-key global-map (kbd "C-+") 'pophint:do-flexibly)
    (define-key global-map (kbd "M-;") 'pophint:redo)
    (define-key global-map (kbd "C-M-;") 'pophint:do-interactively)

    ;; If you want to start some action immediately, bind key for the action.
    (define-key global-map (kbd "M-y") 'pophint:do-flexibly-yank)
    (define-key global-map (kbd "C-M-y") 'pophint:do-rangeyank)

    ;; If you want to use like Vimperator, add key binding for local map.
    (define-key view-mode-map (kbd ";") 'pophint:do-flexibly)
    (add-to-list 'w3m-mode-hook
                 '(lambda () (local-set-key (kbd ";") 'pophint:do-flexibly))
                 t)
    (add-to-list 'Info-mode-hook
                 '(lambda () (local-set-key (kbd ";") 'pophint:do-flexibly))
                 t)

Tested On
=========

* Emacs ... GNU Emacs 23.3.1 (i386-mingw-nt5.1.2600) of 2011-08-15 on GNUPACK
* popup.el ... 0.5.0
* log4e.el ... 0.1
* yaxception.el ... 0.1


**Enjoy!!!**

