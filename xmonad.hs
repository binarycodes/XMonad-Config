-- xmonad.hs
-- Author: sujoy `binarycodes` <sujoy@archlinux.us>
--


-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))
import System.Exit

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
import XMonad.Hooks.EwmhDesktops

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
import XMonad.Layout.LayoutModifier

-- custom
import Binarycodes.StatusBars
import Binarycodes.Keys
import Binarycodes.Config
import Binarycodes.ManageHook
import Binarycodes.Workspaces

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
              , startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
              }



-- layouts

layoutHook' = onWorkspace (getWorkspaceId "main") mainL
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
          web = named "WEB" $  ResizableTall 1 (1/100) (2/3) []

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

          mainL = applyToAllLayouts (tiled ||| combo ||| rft ||| Grid ||| mt ||| Full ||| sFloat)
          webL  = applyToAllLayouts (web ||| tiled ||| Full ||| mt ||| tb ||| sFloat)
          docL  = applyToAllLayouts (mt ||| tiled ||| Full ||| tb ||| sFloat)
          codeL = applyToAllLayouts (combo ||| tiled ||| mt ||| Full ||| Grid ||| sFloat)
          chatL = applyToAllLayouts $ im (Grid ||| mt ||| threeCol ||| tiled ||| sFloat)
          floatL = applyToAllLayouts (sFloat ||| mt ||| threeCol ||| tiled)
          officeL = applyToAllLayouts (tb ||| tiled ||| rft ||| mt ||| sFloat)
          restL = applyToAllLayouts (tiled ||| Full ||| Grid ||| mt ||| sFloat)

          --- My Theme For Tabbed layout
          myTheme = defaultTheme { decoHeight = 14
                                 , fontName = "-*-envy code r-medium-r-*-*-12-*-*-*-*-*-iso8859-*"
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
