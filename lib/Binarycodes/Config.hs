-- Author: sujoy `binarycodes` <sujoy@archlinux.us>
--

module Binarycodes.Config where


-- XMonad
import XMonad (Dimension)




-- Settings for the status bars

currentFG = "#000000"
currentBG = "#9ABD02"

hiddenFG = "#000000"
hiddenBG = "#CAA800"

hiddenNoWinFG = "#000000"
hiddenNoWinBG = "#606060"

titleFG = "#FFA662"
titleBG = "#404040"

layoutFG = "#000000"
layoutBG = "#77C853"

separatorColor = "#444444"

urgentFG = "#000000"
urgentBG = "#FF0000"


-- Dzen --
myDzenFont = "-*-envy code r-medium-r-*-*-11-*-*-*-*-*-iso8859-*"
myDzenEvents = "-e 'button3='"
myWorkspaceBar = "dzen2 -p -ta l -fn '" ++ myDzenFont ++ "' -w 1100 -bg '#404040' -fg '#000000' "
                 ++ myDzenEvents 
myConkyBar = "dzen2 -p -ta r -fn '" ++ myDzenFont ++ "' -x 1100 -w 820 -bg '#404040' -fg '#000000' " 
             ++ myDzenEvents

-- borders
borderWidth' :: Dimension
borderWidth' = 1 
               
normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#606060"
focusedBorderColor' = currentBG

-- Terminal --
terminal' :: String
terminal' = "urxvtc" -- since we already start the urxvt daemon from xinitrc
