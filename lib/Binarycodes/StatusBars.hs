-- Author: sujoy `binarycodes` <sujoy@archlinux.us>
--

module Binarycodes.StatusBars where

-- XMonad
import XMonad
import System.IO (Handle, hPutStrLn)

-- Hooks
import XMonad.Hooks.DynamicLog

-- Custom
import Binarycodes.Config



logHook' :: Handle ->  X ()
logHook' bar = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn bar }

customPP :: PP
customPP = defaultPP { ppCurrent = wrap ("^bg("++ currentBG ++")^fg(" ++ currentFG++ ")")
                                   "^fg()^bg()"
                     , ppTitle = wrap (" ^fg(" ++ titleFG++ ")")
                                 "^fg() " . shorten 80
                     , ppSep = "^bg(" ++ separatorColor ++ ")^r(1,15)^bg()" 
                     , ppWsSep = "^bg(" ++ separatorColor ++ ")^r(2,15)^bg()" 
                     , ppUrgent = wrap ("^bg("++ urgentBG ++")^fg(" ++ urgentFG++ ")")
                                   "^fg()^bg()" . dzenStrip
                     , ppHidden = wrap ("^bg("++ hiddenBG ++")^fg(" ++ hiddenFG++ ")")
                                  "^fg()^bg()"
                     , ppHiddenNoWindows = wrap ("^bg("++ hiddenNoWinBG ++")^fg("
                                                 ++ hiddenNoWinFG ++ ")")
                                           "^fg()^bg()"
                     , ppLayout = wrap ("^bg("++ layoutBG ++") ^fg("
                                        ++ layoutFG ++ ")")
                                  "^fg() ^bg()"
                     }

