# Emacs swift-mode

Needs help. Please send your pull requests.

##Installation

Put `swift-mode.el` somewhere in your load-path

Add the following to your `~/.emacs` file: 

    (require 
        'swift-mode) 
    (setq auto-mode-alist 
        (append 
            '(("\\.swift$" . swift-mode)) 
                auto-mode-alist)) 

 `swift-mode` will now be called whenever a `.swift` file is opened,
 or by executing `M-x swift-mode`

## Copyright
MIT Licence (see LICENCE in this repository)

## To do

Lots! Please send your pull requests.

 - Finish the list of keywords and builtins
 - Auto-indentation for functions, classes and collection literals
 - Highlight variables, type names and constants
 - Evaluate region
 - REPL mode
 - comments
 - Highlight bad syntax
 - Highlight type errors :->
