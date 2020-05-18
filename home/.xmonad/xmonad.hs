-- [[file:~/repos/neurosys/README.org::*Imports][Imports:1]]
import XMonad
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
import XMonad.Layout.Spacing
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco, shrinkText,
                                         inactiveBorderColor, inactiveColor, inactiveTextColor, activeBorderColor,
                                         activeColor, activeTextColor, urgentBorderColor, urgentTextColor, decoHeight)

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
                 spacing 3 $  -- gap between windows
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

-- [[file:~/repos/neurosys/README.org::*Keybindings][Keybindings:1]]
myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys XConfig {modMask = m, terminal = term} = M.fromList $ [
  -- System
    ((m .|. shiftMask .|. mod1Mask, xK_r), spawn "reboot")
  , ((m,                            xK_q), kill)
  -- Launcher
  , ((m,                            xK_p), spawn "rofi -show drun -modi drun -show-icons -matching fuzzy")
  -- Window Search
  , ((m,                            xK_b), spawn "rofi -show window -show-icons -matching fuzzy")
  -- Quick Emacs
  , ((m,                            xK_n), spawn "emacsclient -c")
  , ((m .|. shiftMask,              xK_n), spawn "~/.emacs.d/bin/doom run")
  -- Lock Screen
  , ((m .|. shiftMask .|. mod1Mask, xK_o), spawn "xtrlock -b")
  -- Restart Xmonad
  , ((m .|. shiftMask .|. mod1Mask, xK_i), spawn "xmonad --recompile && xmonad --restart")
  -- Horizontal resizing
  , ((m .|. shiftMask,              xK_h), sendMessage MirrorShrink)
  , ((m .|. shiftMask,              xK_l), sendMessage MirrorExpand)
    --Minimize / restore windows
  , ((m,                            xK_m), withFocused minimizeWindow)
  , ((m .|. shiftMask,              xK_m), sendMessage RestoreNextMinimizedWin)
  -- Fullscreen
  , ((m .|. shiftMask,              xK_f), fullFloatFocused)
  -- Quick swap between workspace - very handy
  , ((m,                            xK_comma), toggleWS)
  -- Move default M-, and M-. to M-S-, and M-S-.
  , ((m .|. shiftMask,              xK_comma), sendMessage (IncMasterN 1))
  , ((m .|. shiftMask,              xK_period), sendMessage (IncMasterN (-1)))
  -- Volume Control
  , ((m .|. shiftMask,              xK_Up), spawn "amixer sset Master 5%+")
  , ((m .|. shiftMask,              xK_Down),spawn "amixer sset Master 5%-")
  ] ++
  -- Bind M-{w, e, r} to switch between monitors
  [((m .|. nilOrShift, key), screenWorkspace sc
          >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_e, xK_w, xK_r] [0..]
       , (f, nilOrShift) <- [(W.view, 0), (W.shift, shiftMask)]]
    where
        fullFloatFocused = withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f
-- Keybindings:1 ends here

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
-- Asthetics:1 ends here
