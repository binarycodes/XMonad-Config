-- xmonad.hs
-- Author: sujoy `binarycodes` <sujoy@archlinux.us>

module Binarycodes.Workspaces where

-- XMonad

import XMonad


-- workspaces
workspaceNames :: [String]
workspaceNames = [ "main", "web", "chat", "doc", "code", "mult", "office", "down", "float" ]

-- custom program to add space before/after every element of a list of strings
addSpace :: [String] -> [String]
addSpace [] = []
addSpace (x:xs) = afterSpace : addSpace xs
    where afterSpace = " " ++ x ++ " "

workspaces' :: [WorkspaceId]
workspaces' = addSpace $ zipWith (++) (map show [1..]) wsnames
    where wsnames = map((:) ':')  workspaceNames
              

getWorkspaceId :: String -> WorkspaceId
getWorkspaceId name = case lookup name (zip workspaceNames workspaces') of
                        Just wsId -> wsId
                        Nothing -> head workspaces'
