-- Author: sujoy `binarycodes` <sujoy@archlinux.us>
--

module Binarycodes.Keys where

-- XMonad
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit

-- Hooks
import XMonad.Hooks.ManageDocks

-- Utils
import XMonad.Util.Run (spawnPipe)

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man
import XMonad.Prompt.Window

-- Layouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ResizableTile


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
    [ ((modMask,               xK_F2    ), shellPrompt defaultXPConfig)
    , ((modMask,               xK_F1    ), manPrompt defaultXPConfig)
    , ((modMask .|. shiftMask, xK_g     ), windowPromptGoto defaultXPConfig)
    , ((modMask .|. shiftMask, xK_b     ), windowPromptBring defaultXPConfig)

    -- kill
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
    , ((modMask              , xK_q     ), spawn myRestart)

    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
