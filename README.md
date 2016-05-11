# Predictive Text Editor

> This is a small command line text editor that learns as you type to improve its autocomplete.

## Installation

In order to install the dependencies for this project, run the following commands:

```bash
cabal update
cabal install --only-dependencies
```

## Run the Editor

The project can either be compiled and executed using the following commands:

```bash
ghc Editor.hs
./Editor
```

or the project can just be run using the following command:

```bash
runhaskell Editor.hs
```

## Initializing the Dictionary

If you would like to start with a pre built dictionary, you can supply a text file for the editor to learn from by running the program with a text file:

```bash
runhaskell Editor someFile.txt
```

## Important Files

#### Editor.hs

Editor.hs contians the application logic of the application as well as the entry point.  This file handles the event loop and works the the editor widget to manipulate the text being edited.

#### MyEdit.hs

MyEdit.hs represents a custom text edit widget that provides an interface for tab completion as well as functions to manipulate the dictionary of used words.

#### Predictor.hs

Predictor.hs provides a simple method of tracking the frequency of each word that has been used so far.  In addition, this file exposes functions to guess the word that will come next given a set of words that have previously appeared.

## TODO

- VIM keybindings
- Saving/opening files
- Actual N-Gram predictor
