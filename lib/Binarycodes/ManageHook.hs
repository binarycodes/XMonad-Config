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
manageHook' = composeAll [
  myManageHook,
  manageHook defaultConfig,
  manageDocks,
  isFullscreen --> doFullFloat
  ]


myManageHook = composeOne $ concat
             [ [ stringProperty "WM_WINDOW_ROLE" =? roleC -?> doIgnore | roleC <- hide ]
             , [ className =? c -?> doFloat | c <- noTile ]
             , [ isDialog -?> doFloat ]
             , [ isFullscreen -?> doFullFloat ]
             , [ className =? webC -?> doF (W.shift $ getWorkspaceId "web")  | webC <- web ]
             , [ className =? officeC -?> doF (W.shift $ getWorkspaceId "office") | officeC <- office ]
             , [ className =? multC -?> doF (W.shift $ getWorkspaceId "mult") | multC <- mult ]
             , [ className =? docC -?> doF (W.shift $ getWorkspaceId "doc")  | docC <- doc ]
             , [ className =? codeC -?> doF (W.shift $ getWorkspaceId "code") | codeC <- code ]
             , [ className =? chatC -?> doF (W.shift $ getWorkspaceId "chat") | chatC <- chat ]
             , [ className =? floatC -?> doF (W.shift $ getWorkspaceId "float") | floatC <- float ]
             , [ className =? downC -?> doF (W.shift $ getWorkspaceId "down") | downC <- down ]
             , [ className =? sinkC -?> unFloat | sinkC <- sink ]
             , [ return True -?> doF W.swapDown ]
             ]
             where
                   unFloat = ask >>= doF . W.sink
                   web  = [ "Seamonkey", "qutebrowser", "Opera", "Firefox", "Addons", "chromium", "Chromium" ]
                   doc  = [ "GV" ,"Evince", "Xchm", "Epdfview", "Zathura", "Chmsee" ]
                   code = [ "Netbeans", "Eclipse" ]
                   chat = [ "Pidgin","Qq" ]
                   float = [ "Gimp" , "Gimp-2.8", "Blender" ]
                   mult = [ "Shotwell", "Geeqie" ]
                   down = [ "Transmission", "Deluge" ]
                   office = [ "libreoffice", "libreoffice-calc", "libreoffice-impress", "libreoffice-startcenter", "LibreOffice 3.3", "libreoffice-impress" ]
                   noTile = [ "mplayer2", "xv", "mpv" ]
                   hide = [ ]
                   sink = [ ]
