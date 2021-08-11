-- [[id:f103b573-5fe1-4ebd-ae35-e6f1a73ecdad][Imports:1]]
import XMonad

import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Grid
import XMonad.Layout.Fullscreen
import XMonad.Layout.Minimize
import XMonad.Actions.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco, shrinkText,
                                         inactiveBorderColor, inactiveColor, inactiveTextColor, activeBorderColor,
                                         activeColor, activeTextColor, urgentBorderColor, urgentTextColor, decoHeight)
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.MultiColumns

import XMonad.Actions.CycleWS (toggleWS)
import qualified XMonad.StackSet as W

import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Util.Run

import Data.Monoid
import Data.Default (def)
import Data.Map as M (fromList,union, Map())
-- Imports:1 ends here

-- [[id:099e2205-fc29-41c0-86d4-ce9ac62437c3][Main:1]]
main :: IO ()
main = xmonad $
  withUrgencyHook NoUrgencyHook $
  ewmh $
  fullscreenSupport def {
    borderWidth = 1
  , focusedBorderColor = blue
  , terminal = "alacritty"
  , layoutHook = smartBorders $  -- no borders for sole windows
                 noFrillsDeco shrinkText topBarTheme $   -- visually mark the focused window with a top bar
                 minimize
                 (ResizableTall 1 (3/100) (1/2) []
                   ||| Mirror (ResizableTall 1 (3/100) (1/2) [])
                   ||| multiCol [1] 1 0.01 (-0.5)
                   ||| noBorders Full
                   ||| simpleTabbed
                   ||| Grid)

  , workspaces = map show $ [1..9] ++ [0 :: Int]
  , modMask = mod4Mask  -- super key as modifier
  , keys = \c -> myKeys c `M.union` keys def c
  -- , manageHook = myManageHook
  , handleEventHook = ewmhDesktopsEventHook
  , startupHook = do
      -- http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-SetWMName.html
      setWMName "LG3D"
      windows $ W.greedyView "1"
  }
-- Main:1 ends here

-- [[id:bdb34094-5606-4b90-9659-27f2cc21831a][Custom:1]]
myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys XConfig {modMask = m, terminal = term} = M.fromList $ [
-- Custom:1 ends here

-- [[id:a4300d94-b7ee-46c8-8649-83003a920e9a][Rebooting / Restarting:1]]
    ((m .|. shiftMask .|. mod1Mask, xK_r), spawn "reboot")
  , ((m .|. shiftMask .|. mod1Mask, xK_i), spawn "xmonad --recompile && xmonad --restart")
-- Rebooting / Restarting:1 ends here

-- [[id:0e983f1b-4867-4ae1-bdd7-0eb1fd599656][Add Workspace 0:1]]
  , ((m, xK_0), windows $ W.greedyView "0")
  , ((m .|. shiftMask, xK_0), windows $ W.shift "0")
-- Add Workspace 0:1 ends here

-- [[id:4a65a200-4d49-4b70-8261-c420289f1d68][Launcher / Window Switcher:1]]
  , ((m, xK_p), spawn "rofi -show drun -modi drun -show-icons -matching fuzzy")
  , ((m .|. shiftMask, xK_p), spawn "GDK_SCALE=2 rofi -show drun -modi drun -show-icons -matching fuzzy")
  , ((m, xK_b), spawn "rofi -show window -show-icons -matching fuzzy")
  -- Like M-y for helm-show-kill-ring in Emacs
  , ((m, xK_y), spawn "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'")
-- Launcher / Window Switcher:1 ends here

-- [[id:03709ef1-9ca7-4593-a5a1-973ed3354cb2][Running Emacs:1]]
  , ((m, xK_n), spawn "emacsclient -c")
  , ((m .|. shiftMask, xK_n), spawn "~/.emacs.d/bin/doom run")
-- Running Emacs:1 ends here

-- [[id:1e954aa0-dc30-40a2-88a4-dd94bd92ba32][Lock Screen:1]]
  , ((m .|. shiftMask .|. mod1Mask, xK_o), spawn "~/scripts/fingerprint-lock.sh")
-- Lock Screen:1 ends here

-- [[id:634eac8e-780e-459c-9048-2b4a86a03d58][Horizontal Resizing:1]]
  , ((m .|. shiftMask, xK_h), sendMessage MirrorShrink)
  , ((m .|. shiftMask, xK_l), sendMessage MirrorExpand)
-- Horizontal Resizing:1 ends here

-- [[id:52db6918-73ac-4ea0-97fa-e2d687579ecf][Window Minimization / Restoration:1]]
  , ((m, xK_m), withFocused minimizeWindow)
  , ((m .|. shiftMask, xK_m), withLastMinimized maximizeWindowAndFocus)
-- Window Minimization / Restoration:1 ends here

-- [[id:24dace8c-4205-4956-b4c5-5223bd999826][Fullscreen:1]]
  , ((m .|. shiftMask, xK_f), withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f)
-- Fullscreen:1 ends here

-- [[id:c3bb35a1-49b5-481d-b6a3-42d0fdae5114][Workspace Swapping:1]]
  , ((m, xK_comma), toggleWS)
-- Workspace Swapping:1 ends here

-- [[id:076000a3-93de-41f3-8768-616f43e6d6bc][Changing number of master windows:1]]
  , ((m .|. shiftMask, xK_comma), sendMessage (IncMasterN 1))
  , ((m .|. shiftMask, xK_period), sendMessage (IncMasterN (-1)))
-- Changing number of master windows:1 ends here

-- [[id:f67e9d5b-2ff4-4200-8fde-dd3a0871ea05][Easier Kill Binding:1]]
  , ((m, xK_q), kill)
-- Easier Kill Binding:1 ends here

-- [[id:49308b27-b641-4e31-b811-b4bad6e740dd][Volume Control:1]]
  , ((m .|. shiftMask, xK_Up), spawn "amixer sset Master 5%+")
  , ((m .|. shiftMask, xK_Down),spawn "amixer sset Master 5%-")
-- Volume Control:1 ends here

-- [[id:be6141e6-18de-4dff-9445-51c2635f5c93][Screenshots:1]]
    ,((0, xK_Print), myScreenshot)
-- Screenshots:1 ends here

-- [[id:be6141e6-18de-4dff-9445-51c2635f5c93][Screenshots:2]]
    ,((m, xK_Print), myScreenshotClipboard)
-- Screenshots:2 ends here

-- [[id:af2e9701-ee14-4de2-b9e7-4944c53e1017][Keyboard:1]]
  , ((m .|. shiftMask, xK_i), spawn "setxkbmap -option 'ctrl:nocaps' && xset r rate 160 60")
-- Keyboard:1 ends here

-- [[id:9e55a839-1bcc-4442-a6b8-98b33f6d39c3][Arandr:1]]
  , ((m, xK_s), spawn "/home/dan/.screenlayout/main.sh && feh --bg-fill --randomize ~/HubbleImages/* &" )
  , ((m .|. shiftMask, xK_s), spawn "/home/dan/.screenlayout/laptop.sh && feh --bg-fill --randomize ~/HubbleImages/* &" )
-- Arandr:1 ends here

-- [[id:f8ac74cc-5ca6-424f-bb20-f4fe05750b65][Multiple Monitors:1]]
  ] ++
  [((m .|. nilOrShift, key), screenWorkspace sc
          >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
       , (f, nilOrShift) <- [(W.view, 0), (W.shift, shiftMask)]]
-- Multiple Monitors:1 ends here

-- [[id:b7cecd28-8036-4451-8700-e19d5e5335cc][Asthetics:1]]
red     = "#dc322f"
blue    = "#268bd2"
yellow  = "#b58900"
inactive  = "#002b36"
active      = blue

topBarTheme = def
    { inactiveBorderColor   = inactive
    , inactiveColor         = inactive
    , inactiveTextColor     = inactive
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = 10
    }


myShellPrompt = def
       { font              = "xft:Hack:pixelsize=30"
       , promptBorderWidth = 1
       , position          = Top
       , height            = 42
       , defaultText       = []
       }
-- Asthetics:1 ends here

-- [[id:0d76798f-6aa5-4d7d-89f7-455289a146b2][Screenshot:1]]
myScreenshot = do
  -- init takes care of the trailing newline character returned by date
  date <- init <$> runProcessWithInput "date" ["+%Y-%m-%d-%H:%M:%S"] []
  AL.launchApp myShellPrompt { defaultText = "~/screenshots/" ++ date ++ ".png"} "maim -s"
-- Screenshot:1 ends here

-- [[id:0d76798f-6aa5-4d7d-89f7-455289a146b2][Screenshot:2]]
myScreenshotClipboard :: X ()
myScreenshotClipboard = spawn  "maim -s | xclip -selection clipboard -t image/png"
-- Screenshot:2 ends here
