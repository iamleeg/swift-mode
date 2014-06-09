# Emacs swift-mode

An Emacs major mode for [Swift](https://developer.apple.com/swift/).  
Provides syntax highlighting, indentation support and a few commands.

##Requirements

You must be using Emacs 24 or newer. The Emacs currently distributed
with OS X is 22.1.1 which is _not_ capable of supporting
`swift-mode`. Consider downloading the latest [Emacs for Mac OS
X](http://emacsformacosx.com/) release, or consult your favourite
package manager.

##Installation

Put `swift-mode.el` somewhere in your load-path

Add the following to your `~/.emacs` file: 

    (require 
        'swift-mode) 

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
 - Highlight bad syntax
 - Highlight type errors :->
