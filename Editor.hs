{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Control.Lens
import qualified Graphics.Vty as V
import qualified Predictor as P

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core (str, vBox, hBox)
import MyEdit as E
import qualified Brick.AttrMap as A
import Brick.Widgets.Border as B
import Brick.Util()

import System.Environment (getArgs)

safeHead :: [a] -> Maybe a
safeHead lst
    | null lst  = Nothing
    | otherwise = Just $ head lst

data St = St {
    _edit :: E.Editor,
    _dict :: P.Dictionary
}

makeLenses ''St

drawUI :: St -> [T.Widget]
drawUI st = [vBox [E.renderEditor $ st^.edit,
                   B.hBorder, 
                   hBox [str ("thing: " ++ show (st^.dict))],
                   hBox [str ("Suggestions: " ++ joined)]]]
    where fixedWords = take 3 $ getSuggestions st
          joined     = intercalate ", " fixedWords

appEvent :: St -> V.Event -> T.EventM (T.Next St)
appEvent st ev =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar ' ') [] -> M.continue =<< T.handleEventLensed newSt edit ev
            where newSt = addWord (E.lastWord (st ^. edit)) st
        V.EvKey (V.KChar '\t') [] -> M.continue $ Main.completeWord headWord $ addWord headWord st
            where fixedWords = take 3 $ getSuggestions st
                  headWord   = fromMaybe "" $ safeHead fixedWords
        _ -> M.continue =<< T.handleEventLensed st edit ev

addWord :: String -> St -> St
addWord new st = st & dict .~ P.addWord new (st ^. dict)

completeWord :: String -> St -> St
completeWord new st = st & edit .~ E.completeWord new (st ^. edit)

getSuggestions :: St -> [String]
getSuggestions st = P.getMatchedWords (P.fixString $ E.lastWord (st ^. edit)) (st ^. dict)

initialState :: P.Dictionary -> St
initialState dict = St {
    _edit = E.editor "editor" (str . unlines) Nothing "",
    _dict = dict
}

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []

appCursor :: St -> [T.CursorLocation] -> Maybe T.CursorLocation
appCursor _ = M.showCursorNamed "editor"

theApp :: M.App St V.Event
theApp =
    M.App {
        M.appDraw = drawUI,
        M.appChooseCursor = appCursor, 
        M.appHandleEvent = appEvent, 
        M.appStartEvent = return, 
        M.appAttrMap = const theMap, 
        M.appLiftVtyEvent = id
    }

main :: IO ()
main = do
    args <- getArgs
    print args

    if (not . null) args
        then do
            contents <- readFile $ head args
            _ <- M.defaultMain theApp $ initialState $ P.initialize contents
            putStrLn "done"
        else do
            _ <- M.defaultMain theApp $ initialState M.empty
            putStrLn "done"

