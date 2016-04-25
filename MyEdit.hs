{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module provides a basic text editor widget. You'll need to
-- embed an 'Editor' in your application state and transform it with
-- 'handleEvent' when relevant events arrive. To get the contents
-- of the editor, just use 'getEditContents'. To modify it, use the
-- 'Z.TextZipper' interface with 'applyEdit'.
--
-- The editor's 'HandleEvent' instance handles a set of basic input
-- events that should suffice for most purposes; see the source for a
-- complete list.
module MyEdit
  ( Editor(editContents, editorName, editDrawContents)
  -- * Constructing an editor
  , editor
  -- * Reading editor contents
  , getEditContents
  -- * Editing text
  , applyEdit
  -- * Lenses for working with editors
  , editContentsL
  , editDrawContentsL
  -- * Rendering editors
  , renderEditor
  -- * Attributes
  , editAttr
  , lastWord
  , completeWord
  )
where

import Data.List
import Control.Lens
import Graphics.Vty (Event(..), Key(..), Modifier(..))

import qualified Data.Text.Zipper as Z

import Brick.Types
import Brick.Widgets.Core
import Brick.AttrMap

-- | Editor state.  Editors support the following events by default:
--
-- * Ctrl-a: go to beginning of line
-- * Ctrl-e: go to end of line
-- * Ctrl-d, Del: delete character at cursor position
-- * Backspace: delete character prior to cursor position
-- * Ctrl-k: delete all from cursor to end of line
-- * Arrow keys: move cursor
-- * Enter: break the current line at the cursor position
data Editor =
    Editor { editContents :: Z.TextZipper String
           -- ^ The contents of the editor
           , editDrawContents :: [String] -> Widget
           -- ^ The function the editor uses to draw its contents
           , editorName :: Name
           -- ^ The name of the editor
           }

suffixLenses ''Editor

insertStr :: String -> Z.TextZipper String -> Z.TextZipper String
insertStr st z = foldl (flip Z.insertChar) z st

insertPair :: Char -> Char -> Z.TextZipper String -> Z.TextZipper String
insertPair o c z = Z.moveLeft $ Z.insertChar c $ Z.insertChar o z

instance HandleEvent Editor where
    handleEvent e ed =
        let f = case e of
                    EvKey (KChar 'a') [MCtrl] -> Z.gotoBOL
                    EvKey (KChar 'e') [MCtrl] -> Z.gotoEOL
                    EvKey (KChar 'd') [MCtrl] -> Z.deleteChar
                    EvKey (KChar 'k') [MCtrl] -> Z.killToEOL
                    EvKey (KChar '\t') []     -> insertStr "    "
                    EvKey (KChar '(') []      -> insertPair '(' ')'
                    EvKey (KChar '[') []      -> insertPair '[' ']'
                    EvKey (KChar '{') []      -> insertPair '{' '}'
                    EvKey (KChar '"') []      -> insertPair '"' '"'
                    EvKey (KChar '\'') []      -> insertPair '\'' '\''
                    EvKey (KChar c) []        -> Z.insertChar c
                    EvKey KDel []             -> Z.deleteChar
                    EvKey KEnter []           -> Z.breakLine
                    EvKey KUp []              -> Z.moveUp
                    EvKey KDown []            -> Z.moveDown
                    EvKey KLeft []            -> Z.moveLeft
                    EvKey KRight []           -> Z.moveRight
                    EvKey KBS []              -> Z.deletePrevChar
                    _ -> id
        in return $ applyEdit f ed

-- | Construct an editor.
editor :: Name
       -- ^ The editor's name (must be unique)
       -> ([String] -> Widget)
       -- ^ The content rendering function
       -> Maybe Int
       -- ^ The limit on the number of lines in the editor ('Nothing'
       -- means no limit)
       -> String
       -- ^ The initial content
       -> Editor
editor name draw limit s = Editor (Z.stringZipper [s] limit) draw name

-- | Apply an editing operation to the editor's contents. Bear in mind
-- that you should only apply zipper operations that operate on the
-- current line; the editor will only ever render the first line of
-- text.
applyEdit :: (Z.TextZipper String -> Z.TextZipper String)
          -- ^ The 'Data.Text.Zipper' editing transformation to apply
          -> Editor
          -> Editor
applyEdit f e = e & editContentsL %~ f

completeWord :: String -> Editor -> Editor
completeWord "" e   = e
completeWord comp e = e & editContentsL %~ (Z.insertChar ' ' . insertStr (comp \\ lastWord e))

lastWord :: Editor -> String
lastWord e = case contents of
                [] -> ""
                _ -> last contents
    where contents = words $ head $ Z.getText $ e ^. editContentsL

-- | The attribute assigned to the editor
editAttr :: AttrName
editAttr = "edit"

-- | Get the contents of the editor.
getEditContents :: Editor -> [String]
getEditContents e = Z.getText $ e ^. editContentsL

-- | Turn an editor state value into a widget
renderEditor :: Editor -> Widget
renderEditor e =
    let cp = Z.cursorPosition $ e^.editContentsL
        cursorLoc = Location (cp ^. _2, cp ^. _1)
        limit = case e ^. editContentsL.to Z.getLineLimit of
            Nothing -> id
            Just lim -> vLimit lim
    in withAttr editAttr $
       limit $
       viewport (e^.editorNameL) Both $
       showCursor (e^.editorNameL) cursorLoc $
       visibleRegion cursorLoc (1, 1) $
       e^.editDrawContentsL $
       getEditContents e
