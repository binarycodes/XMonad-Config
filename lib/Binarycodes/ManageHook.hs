-- xmonad.hs
-- Author: sujoy `binarycodes` <sujoy@archlinux.us>

module Binarycodes.ManageHook where

-- XMonad
import XMonad
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- Layout
import XMonad.Layout.PerWorkspace (onWorkspace)

-- Custom
import Binarycodes.Workspaces


-- Hooks --
manageHook' :: ManageHook
manageHook' = myManageHook <+> manageHook defaultConfig <+> manageDocks <+> (doF W.swapDown)

myManageHook = composeAll $ concat
             [ [ stringProperty "WM_WINDOW_ROLE" =? roleC --> doIgnore | roleC <- hide ]
             , [ className =? webC --> doF (W.shift $ getWorkspaceId "web")  | webC <- web ]
             , [ className =? officeC --> doF (W.shift $ getWorkspaceId "office") | officeC <- office ]
             , [ className =? mailC --> doF (W.shift $ getWorkspaceId "mail") | mailC <- mail ]
             , [ className =? docC --> doF (W.shift $ getWorkspaceId "doc")  | docC <- doc ]
             , [ className =? codeC --> doF (W.shift $ getWorkspaceId "code") | codeC <- code ]
             , [ className =? chatC --> doF (W.shift $ getWorkspaceId "chat") | chatC <- chat ]
             , [ className =? floatC --> doF (W.shift $ getWorkspaceId "float") | floatC <- float ]
             , [ className =? downC --> doF (W.shift $ getWorkspaceId "down") | downC <- down ]
             , [ isDialog --> doIgnore ]
             ]
             where web  = [ "Namoroka", "Jumanji", "Opera", "Firefox", "Chromium", "Kazehakase" ]
                   doc  = [ "GV" ,"Evince", "Xchm", "Epdfview", "Zathura", "Chmsee" ]
                   code = [ "Netbeans" ]
                   chat = [ "Pidgin","Qq" ]
                   float = [ "Gimp" , "Blender" ]
                   mail = [ "Lanikai", "Liferea-bin" ]
                   down = [ "Transmission", "Deluge" ]
                   office = [ "libreoffice-calc", "libreoffice-startcenter", "LibreOffice 3.3" ]
                   hide = [ ]

