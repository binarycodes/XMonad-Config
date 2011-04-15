-- xmonad.hs
-- Author: sujoy `binarycodes` <sujoy@archlinux.us>
--
-- Tested for Xmonad 0.8.1
--

-------------------------------------------------------------------------------
-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))
import System.Exit
import System.IO (Handle, hPutStrLn)

-- utils
import XMonad.Util.Run (spawnPipe)
import XMonad.Prompt
import XMonad.Prompt.Shell

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.ManageHook

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Reflect
import XMonad.Layout.Combo
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Named
import XMonad.Layout.TwoPane

-------------------------------------------------------------------------------
-- Main --
main = do
       bar <- spawnPipe myWorkspaceBar
       spawn ("conky -c ~/.xmonad/dzenConky.rc | "++myConkyBar)
       xmonad $ withUrgencyHook NoUrgencyHook
              $ defaultConfig
              { workspaces = workspaces'
              , modMask = modMask'
              , borderWidth = borderWidth'
              , normalBorderColor = normalBorderColor'
              , focusedBorderColor = focusedBorderColor'
              , terminal = terminal'
              , keys = keys'
              , logHook = logHook' bar
              , layoutHook = layoutHook'
              , manageHook = manageHook'
              , startupHook = setWMName "LG3D"
              }

-------------------------------------------------------------------------------
-- Dzen --
myDzenFont = "Pragmata:pixelsize=12"
myDzenEvents = "-e 'button3='"
myWorkspaceBar = "dzen2 -p -ta l -fn '" ++ myDzenFont ++ "' -w 1100 -bg '#404040' -fg '#000000' "
                 ++ myDzenEvents 
myConkyBar = "dzen2 -p -ta r -fn '" ++ myDzenFont ++ "' -x 1100 -w 820 -bg '#404040' -fg '#000000' " 
             ++ myDzenEvents

-------------------------------------------------------------------------------
-- Hooks --
manageHook' :: ManageHook
manageHook' = myManageHook <+> manageHook defaultConfig <+> manageDocks <+> (doF W.swapDown)

myManageHook = composeAll $ concat
             [ [ stringProperty "WM_WINDOW_ROLE" =? roleC --> doIgnore | roleC <- hide ]
             , [ className =? webC --> doF (W.shift $ getWorkspaceId "web")  | webC <- web ]
             , [ className =? mailC --> doF (W.shift $ getWorkspaceId "mail") | mailC <- mail ]
             , [ className =? docC --> doF (W.shift $ getWorkspaceId "doc")  | docC <- doc ]
             , [ className =? codeC --> doF (W.shift $ getWorkspaceId "code") | codeC <- code ]
             , [ className =? chatC --> doF (W.shift $ getWorkspaceId "chat") | chatC <- chat ]
             , [ className =? floatC --> doF (W.shift $ getWorkspaceId "float") | floatC <- float ]
             , [ className =? downC --> doF (W.shift $ getWorkspaceId "down") | downC <- down ]
             ]
             where web  = [ "Namoroka", "Jumanji", "Opera", "Firefox", "Chromium", "Kazehakase" ]
                   doc  = [ "GV" ,"Evince", "Xchm", "Epdfview", "Zathura", "Chmsee" ]
                   code = [ "Netbeans" ]
                   chat = [ "Pidgin","Qq" ]
                   float = [ "Gimp" , "Blender" ]
                   mail = [ "Lanikai", "Liferea-bin" ]
                   down = [ "Transmission", "Deluge", "Download" ]
                   hide = [ ]


logHook' :: Handle ->  X ()
logHook' bar = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn bar }

layoutHook' = customLayout

-------------------------------------------------------------------------------
-- Looks --
-- bar

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

-- borders
borderWidth' :: Dimension
borderWidth' = 1 
               
normalBorderColor', focusedBorderColor' :: String
normalBorderColor'  = "#606060"
focusedBorderColor' = currentBG

-- workspaces
workspaceNames :: [String]
workspaceNames = [ "main", "web", "chat", "doc", "code", "mail", "office", "down", "float" ]

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

-- layouts
customLayout = onWorkspace (getWorkspaceId "main") mainL
             $ onWorkspace (getWorkspaceId "web") webL
             $ onWorkspace (getWorkspaceId "doc") docL
             $ onWorkspace (getWorkspaceId "chat") chatL
             $ onWorkspace (getWorkspaceId "code") codeL
             $ onWorkspace (getWorkspaceId "float") floatL
             $ onWorkspace (getWorkspaceId "office") officeL
             $ restL
    where tiled = named "RT" $ ResizableTall 1 (1/100) (1/2) []
          rft = named "RFT" $ (reflectHoriz tiled)
          threeCol = named "3C" $ ThreeCol 1 (1/100) (1/2)
          combo = named "CB" $ combineTwo tp mt tb
          tb = named "TB" $ tabbedBottom shrinkText myTheme
          sFloat = named "SF" $ simplestFloat
          mt = named "MT" $ Mirror tiled
          tp = named "TP" $ TwoPane (3/100) (1/2)

          im layoutL = named "IM" $ withIM ratio pidginRoster $ reflectHoriz $ withIM
               skypeRatio skypeRoster layoutL
          ratio = (1%9)
          skypeRatio = (1%8)
          pidginRoster = And (ClassName "Pidgin") (Role "buddy_list")
          skypeRoster = (ClassName "Skype")
                        `And` (Not (Title "Options"))
                        `And` (Not (Role "Chats"))
                        `And` (Not (Role "CallWindowForm"))

          applyToAllLayouts layoutList = avoidStruts $ smartBorders $
                                         windowNavigation layoutList 
          mainL = applyToAllLayouts (combo ||| tiled ||| rft ||| Grid ||| mt ||| Full)
          webL  = applyToAllLayouts (Full ||| mt ||| tiled ||| tb)
          docL  = applyToAllLayouts (mt ||| tiled ||| Full ||| tb)
          codeL = applyToAllLayouts (combo ||| tiled ||| mt ||| Full ||| Grid)
          chatL = applyToAllLayouts $ im (Grid ||| mt ||| threeCol ||| tiled)
          floatL = applyToAllLayouts (sFloat ||| mt ||| threeCol ||| tiled)
          officeL = applyToAllLayouts (tp ||| tiled ||| rft ||| mt)
          restL = applyToAllLayouts (tiled ||| Full ||| Grid ||| mt)

          --- My Theme For Tabbed layout
          myTheme = defaultTheme { decoHeight = 18
                                 , fontName = "xft:Pragmata:pixelsize=12"
                                 , activeColor = "#799500"
                                 , activeBorderColor = "#799500"
                                 , activeTextColor = "#000000"
                                 , inactiveBorderColor = "#444444"
                                 , inactiveColor = "#606060"
                                 , inactiveTextColor = "#000000"
                                 , urgentColor = "#ff0000"
                                 , urgentBorderColor = "#ff0000"
                                 , urgentTextColor = "#000000"
                                 }

-------------------------------------------------------------------------------
-- Terminal --
terminal' :: String
terminal' = "urxvtc"

-- Dmenu stuffs --
myBarFont :: String
myBarFont = "-*-profont-*-*-*-*-11-*-*-*-*-*-*-*"

myFocsFG, myFocsBG :: String
myFocsFG = "#000000" -- focused foreground colour
myFocsBG = "#999999" -- focused background colour

myNormFG, myNormBG :: String
myNormFG = "#8ba574" -- normal foreground colour
myNormBG = "#000000" -- normal background colour

myDmenuCmd :: String
myDmenuCmd = "dmenu_path | dmenu -i -p 'Run:'" ++ myDmenuOpts
    where myDmenuOpts = concatMap ((:) ' '. (:) '-')
                        [ wrap "nf '" "'" myNormFG
                        , wrap "nb '" "'" myNormBG
                        , wrap "sf '" "'" myFocsFG
                        , wrap "sb '" "'" myFocsBG
                        , wrap "fn '" "'" myBarFont ]

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' :: KeyMask
modMask' = mod4Mask


-- kill conkys and dzen2s for they will be restarted
myRestart :: String
myRestart = "for pid in `pgrep conky`; do kill -9 $pid; done && "
            ++ "for pid in `pgrep dzen2`; do kill -9 $pid; done && "
            ++ "xmonad --recompile && xmonad --restart"

-- keys
keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
-- launching and killing programs
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask,               xK_p     ), spawn $ "exe=`" ++ myDmenuCmd
                                             ++ "` && eval \"exec $exe\"")
    , ((modMask,               xK_F2    ), shellPrompt defaultXPConfig)
    , ((modMask .|. shiftMask, xK_c     ), kill)

    -- layouts
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask,               xK_b     ), sendMessage ToggleStruts)

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,               xK_n     ), refresh)

    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- swap...
    , ((modMask .|. controlMask, xK_Right), sendMessage $ Swap R)
    , ((modMask .|. controlMask, xK_Left ), sendMessage $ Swap L)
    , ((modMask .|. controlMask, xK_Up   ), sendMessage $ Swap U)
    , ((modMask .|. controlMask, xK_Down ), sendMessage $ Swap D)
      
    -- move
    , ((modMask .|. shiftMask, xK_Right), sendMessage $ Move R)
    , ((modMask .|. shiftMask, xK_Left ), sendMessage $ Move L)
    , ((modMask .|. shiftMask, xK_Up   ), sendMessage $ Move U)
    , ((modMask .|. shiftMask, xK_Down ), sendMessage $ Move D)

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    --, ((modMask              , xK_q     ), restart "xmonad" True)
    , ((modMask              , xK_q     ), spawn myRestart)

    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

-------------------------------------------------------------------------------
