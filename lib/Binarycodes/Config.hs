-- Author: sujoy `binarycodes` <sujoy@archlinux.us>
--

module Binarycodes.Config where


-- XMonad
import XMonad (Dimension)




-- Settings for the status bars

currentFG = "#000000"
currentBG = "#B4D32C"

hiddenFG = "#000000"
hiddenBG = "#979E2E"

hiddenNoWinFG = "#000000"
hiddenNoWinBG = "#606060"

titleFG = "#43F67C"
titleBG = "#404040"

layoutFG = "#000000"
layoutBG = "#77C853"

separatorColor = "#444444"

urgentFG = "#000000"
urgentBG = "#FF0000"


-- Dzen --
myDzenFont = "-*-droid sans mono-*-r-*-*-10-*-*-*-*-*-iso8859-*"
myDzenEvents = "-e 'button3='"
myWorkspaceBar = "dzen2 -dock -p -ta l -fn '" ++ myDzenFont ++ "' -w 1100 -bg '#404040' -fg '#000000' "
                 ++ myDzenEvents 
myConkyBar = "dzen2 -dock -p -ta r -fn '" ++ myDzenFont ++ "' -x 1100 -w 820 -bg '#404040' -fg '#000000' " 
             ++ myDzenEvents

-- borders
borderWidth' :: Dimension
borderWidth' = 1 
               
normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#606060"
focusedBorderColor' = currentBG

-- Terminal --
terminal' :: String
terminal' = "termite" -- since we already start the urxvt daemon from xinitrc
