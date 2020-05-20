-- [[file:~/repos/neurosys/README.org::*Imports][Imports:1]]
import XMonad
import XMonad.Core
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Layout.Minimize
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco, shrinkText,
                                         inactiveBorderColor, inactiveColor, inactiveTextColor, activeBorderColor,
                                         activeColor, activeTextColor, urgentBorderColor, urgentTextColor, decoHeight)

import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Util.Run

import Data.Monoid
import Data.Default (def)
import Data.Map as M (fromList,union, Map())
-- Imports:1 ends here

-- [[file:~/repos/neurosys/README.org::*Main][Main:1]]
main :: IO ()
main = xmonad $
  withUrgencyHook NoUrgencyHook $
  ewmh $
  fullscreenSupport def {
    borderWidth = 1
  , focusedBorderColor = blue
  , terminal = "emacsclient -c -e \"(vterm)\""  --assumes emacs server running
  , layoutHook = smartBorders $  -- no borders for sole windows
                 noFrillsDeco shrinkText topBarTheme $   -- visually mark the focused window with a top bar
                 minimize
                 (ResizableTall 1 (3/100) (1/2) []
                   ||| Mirror (ResizableTall 1 (3/100) (1/2) [])
                   ||| noBorders Full
                   ||| Grid)
  , workspaces = map show $ [1..9] ++ [0 :: Int]
  , modMask = mod4Mask  -- super key as modifier
  , keys = \c -> myKeys c `M.union` keys def c
  , handleEventHook = ewmhDesktopsEventHook
  , startupHook = do
      -- http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-SetWMName.html
      setWMName "LG3D"
      windows $ W.greedyView "1"
  }
-- Main:1 ends here

-- [[file:~/repos/neurosys/README.org::*Custom][Custom:1]]
myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys XConfig {modMask = m, terminal = term} = M.fromList $ [
-- Custom:1 ends here

-- [[file:~/repos/neurosys/README.org::*Rebooting / Restarting][Rebooting / Restarting:1]]
    ((m .|. shiftMask .|. mod1Mask, xK_r), spawn "reboot")
  , ((m .|. shiftMask .|. mod1Mask, xK_i), spawn "xmonad --recompile && xmonad --restart")
-- Rebooting / Restarting:1 ends here

-- [[file:~/repos/neurosys/README.org::*Launcher / Window Switcher][Launcher / Window Switcher:1]]
  , ((m, xK_p), spawn "rofi -show drun -modi drun -show-icons -matching fuzzy")
  , ((m, xK_b), spawn "rofi -show window -show-icons -matching fuzzy")
-- Launcher / Window Switcher:1 ends here

-- [[file:~/repos/neurosys/README.org::*Running Emacs][Running Emacs:1]]
  , ((m, xK_n), spawn "emacsclient -c")
  , ((m .|. shiftMask, xK_n), spawn "~/.emacs.d/bin/doom run")
-- Running Emacs:1 ends here

-- [[file:~/repos/neurosys/README.org::*Lock Screen][Lock Screen:1]]
  , ((m .|. shiftMask .|. mod1Mask, xK_o), spawn "xtrlock -b")
-- Lock Screen:1 ends here

-- [[file:~/repos/neurosys/README.org::*Horizontal Resizing][Horizontal Resizing:1]]
  , ((m .|. shiftMask, xK_h), sendMessage MirrorShrink)
  , ((m .|. shiftMask, xK_l), sendMessage MirrorExpand)
-- Horizontal Resizing:1 ends here

-- [[file:~/repos/neurosys/README.org::*Window Minimization / Restoration][Window Minimization / Restoration:1]]
  , ((m, xK_m), withFocused minimizeWindow)
  , ((m .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
-- Window Minimization / Restoration:1 ends here

-- [[file:~/repos/neurosys/README.org::*Fullscreen][Fullscreen:1]]
  , ((m .|. shiftMask, xK_f), withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f)
-- Fullscreen:1 ends here

-- [[file:~/repos/neurosys/README.org::*Workspace Swapping][Workspace Swapping:1]]
  , ((m, xK_comma), toggleWS)
-- Workspace Swapping:1 ends here

-- [[file:~/repos/neurosys/README.org::*Changing number of master windows][Changing number of master windows:1]]
  , ((m .|. shiftMask, xK_comma), sendMessage (IncMasterN 1))
  , ((m .|. shiftMask, xK_period), sendMessage (IncMasterN (-1)))
-- Changing number of master windows:1 ends here

-- [[file:~/repos/neurosys/README.org::*Easier Kill Binding][Easier Kill Binding:1]]
  , ((m, xK_q), kill)
-- Easier Kill Binding:1 ends here

-- [[file:~/repos/neurosys/README.org::*Volume Control][Volume Control:1]]
  , ((m .|. shiftMask, xK_Up), spawn "amixer sset Master 5%+")
  , ((m .|. shiftMask, xK_Down),spawn "amixer sset Master 5%-")
-- Volume Control:1 ends here

-- [[file:~/repos/neurosys/README.org::*Screenshots][Screenshots:1]]
  , ((0, xK_Print), myScreenshot)
-- Screenshots:1 ends here

-- [[file:~/repos/neurosys/README.org::*Screenshots][Screenshots:2]]
  , ((m, xK_Print), myScreenshotClipboard)
-- Screenshots:2 ends here

-- [[file:~/repos/neurosys/README.org::*Multiple Monitors][Multiple Monitors:1]]
  ] ++
  [((m .|. nilOrShift, key), screenWorkspace sc
          >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
       , (f, nilOrShift) <- [(W.view, 0), (W.shift, shiftMask)]]
-- Multiple Monitors:1 ends here

-- [[file:~/repos/neurosys/README.org::*Asthetics][Asthetics:1]]
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
    , decoHeight            = 5
    }


myShellPrompt = def
       { font              = "xft:Hack:pixelsize=30"
       , promptBorderWidth = 1
       , position          = Top
       , height            = 42
       , defaultText       = []
       }
-- Asthetics:1 ends here

-- [[file:~/repos/neurosys/README.org::*Screenshot][Screenshot:1]]
myScreenshot = do
  -- init takes kare of the trailing newline character returned by date
  date <- init <$> runProcessWithInput "date" ["+%Y-%m-%d-%H:%M:%S"] []
  AL.launchApp myShellPrompt { defaultText = "~/screenshots/" ++ date ++ ".png"} "maim -s"
-- Screenshot:1 ends here

-- [[file:~/repos/neurosys/README.org::*Screenshot][Screenshot:2]]
myScreenshotClipboard = spawn  "maim -s | xclip -selection clipboard -t image/png"
-- Screenshot:2 ends here
