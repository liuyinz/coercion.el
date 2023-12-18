# coercion.el

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg?style=flat)](LICENSE)
[![MELPA](https://melpa.org/packages/coercion-badge.svg)](https://melpa.org/#/coercion)

Switch naming convention style quickly. Inspired by [vim-abolish](https://github.com/tpope/vim-abolish#coercion)

<!-- markdown-toc start -->

## Contents

- [coercion.el](#coercionel)
  - [Screenshot](#screenshot)
  - [Install](#install)
    - [dependencies](#dependencies)
    - [package](#package)
  - [Usage](#usage)
  - [customization](#customization)
  - [Comparison](#comparison)

<!-- markdown-toc end -->

## Screenshot

- Call `M-x coercion-*` related command

![example](example.gif)

## Install

### dependencies

- Emacs, version >= 29.1

### package

- Manually

Clone and add to `load-path`, require the package.

- Melpa

**WARNING: not available yet.**

~~This package is available on [MELPA].~~

~~Install with `M-x package-install` `RET` `coercion` within Emacs.~~

## Usage

```elisp
(require 'coercion)
(keymap-global-set "C-c e" coercion-command-map)

;; or bind commands directly
(keymap-global-set "C-c C-j" #'coercion-camel-case)
```

## customization

Define new command by providing `split` and `join` rules.

- `coercion--change (&key split join)`
  This function accept two keywords arguments:
  - :split
    - a function accepts a string as arg, return a list of strings split
    - a regexp string
    - if nil, use default regex
  - :join
    - a function accepts a list of strings as arg, return joined string
    - a list of (function1 function2 ... separator), which all functions would
      be applied to every string in the list, and joined with separator at last.

e.g.

```elisp
(defun coercion-pascal-case ()
  "Convert to pascal-case style."
  (interactive)
  (coercion--change :join '(capitalize "")))

(defun coercion-camel-case ()
  "Convert to camel-case style."
  (interactive)
  (coercion--change :join #'coercion--join-camel-case))
```

## Comparison

- [string-inflection](https://github.com/akicho8/string-inflection)
  - no stand-alone commands, but switch style in pair.
  - provide cycle commands for major-modes which could be implemented in coercion later
