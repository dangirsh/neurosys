-- [[file:../../README.org::*Imports][Imports:1]]
import XMonad

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicProperty (dynamicTitle)
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
import XMonad.Actions.CycleRecentWS (cycleRecentWS)
import qualified XMonad.StackSet as W

import XMonad.Prompt
import XMonad.Prompt.AppLauncher as AL
import XMonad.Util.Run

import Data.Monoid
import Data.Default (def)
import Data.Map as M (fromList,union, Map())
import Data.List (isPrefixOf)
-- Imports:1 ends here

-- [[file:../../README.org::*Main][Main:1]]
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
  , manageHook = myManageHook <+> manageZoomHook
  , handleEventHook = ewmhDesktopsEventHook <+> myHandleEventHook
  , startupHook = do
      -- http://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-SetWMName.html
      setWMName "LG3D"
      windows $ W.greedyView "1"
  }
-- Main:1 ends here

-- [[file:../../README.org::*Custom][Custom:1]]
myKeys :: XConfig t -> M.Map (KeyMask, KeySym) (X ())
myKeys XConfig {modMask = m, terminal = term} = M.fromList $ [
-- Custom:1 ends here

-- [[file:../../README.org::*Rebooting / Restarting][Rebooting / Restarting:1]]
    ((m .|. shiftMask .|. mod1Mask, xK_r), spawn "reboot")
  , ((m .|. shiftMask .|. mod1Mask, xK_i), spawn "xmonad --recompile && xmonad --restart")
-- Rebooting / Restarting:1 ends here

-- [[file:../../README.org::*Add Workspace 0][Add Workspace 0:1]]
  , ((m, xK_0), windows $ W.greedyView "0")
  , ((m .|. shiftMask, xK_0), windows $ W.shift "0")
-- Add Workspace 0:1 ends here

-- [[file:../../README.org::*Launcher / Window Switcher][Launcher / Window Switcher:1]]
  , ((m, xK_p), spawn "rofi -show drun -modi drun -show-icons -matching fuzzy")
  , ((m .|. shiftMask, xK_p), spawn "GDK_SCALE=2 rofi -show drun -modi drun -show-icons -matching fuzzy")
  , ((m, xK_b), spawn "rofi -show window -show-icons -matching fuzzy")
  -- Like M-y for helm-show-kill-ring in Emacs
  , ((m, xK_y), spawn "rofi -modi \"clipboard:greenclip print\" -show clipboard -run-command '{cmd}'")
  -- Text espander

  , ((m .|. shiftMask .|. mod1Mask, xK_j), spawn "texpander.sh")
  , ((m .|. shiftMask .|. mod1Mask, xK_p), spawn "rofipass")
-- Launcher / Window Switcher:1 ends here

-- [[file:../../README.org::*Running Emacs][Running Emacs:1]]
  , ((m, xK_n), spawn "emacsclient -c")
  , ((m .|. shiftMask .|. mod1Mask, xK_n), spawn "EMACS_NON_WORK_MODE=1 ~/scripts/run_emacs.sh")
  , ((m .|. shiftMask, xK_n), spawn "~/scripts/run_emacs.sh")
-- Running Emacs:1 ends here

-- [[file:../../README.org::*Lock Screen][Lock Screen:1]]
  , ((m .|. shiftMask .|. mod1Mask, xK_o), spawn "xtrlock -b")
-- Lock Screen:1 ends here

-- [[file:../../README.org::*Horizontal Resizing][Horizontal Resizing:1]]
  , ((m .|. shiftMask, xK_h), sendMessage MirrorShrink)
  , ((m .|. shiftMask, xK_l), sendMessage MirrorExpand)
-- Horizontal Resizing:1 ends here

-- [[file:../../README.org::*Window Minimization / Restoration][Window Minimization / Restoration:1]]
  , ((m, xK_m), withFocused minimizeWindow)
  , ((m .|. shiftMask, xK_m), withLastMinimized maximizeWindowAndFocus)
-- Window Minimization / Restoration:1 ends here

-- [[file:../../README.org::*Fullscreen][Fullscreen:1]]
  , ((m .|. shiftMask, xK_f), withFocused $ \f -> windows =<< appEndo `fmap` runQuery doFullFloat f)
-- Fullscreen:1 ends here

-- [[file:../../README.org::*Workspace Swapping][Workspace Swapping:1]]
  , ((m, xK_comma), toggleWS)
-- Workspace Swapping:1 ends here

-- [[file:../../README.org::*Changing number of master windows][Changing number of master windows:1]]
  , ((m .|. shiftMask, xK_comma), sendMessage (IncMasterN 1))
  , ((m .|. shiftMask, xK_period), sendMessage (IncMasterN (-1)))
-- Changing number of master windows:1 ends here

-- [[file:../../README.org::*Easier Kill Binding][Easier Kill Binding:1]]
  , ((m, xK_q), kill)
-- Easier Kill Binding:1 ends here

-- [[file:../../README.org::*Volume Control][Volume Control:1]]
  , ((m .|. shiftMask, xK_Up), spawn "amixer -c 1 -q set Master 2dB+ unmute")
  , ((m .|. shiftMask, xK_Down),spawn "amixer -c 1 -q set Master 2dB- unmute")
-- Volume Control:1 ends here

-- [[file:../../README.org::*Screenshots][Screenshots:1]]
    ,((0, xK_Print), myScreenshot)
-- Screenshots:1 ends here

-- [[file:../../README.org::*Screenshots][Screenshots:2]]
    ,((m, xK_Print), myScreenshotClipboard)
-- Screenshots:2 ends here

-- [[file:../../README.org::*Keyboard][Keyboard:1]]
  , ((m .|. shiftMask, xK_i), spawn "setxkbmap -option 'ctrl:nocaps' && xset r rate 160 80")
-- Keyboard:1 ends here

-- [[file:../../README.org::*Arandr][Arandr:1]]
  , ((m, xK_s), spawn "/home/dan/.screenlayout/main.sh && feh --bg-fill --no-xinerama --randomize ~/Media/images/wallpaper/* &" )
  , ((m .|. shiftMask, xK_s), spawn "/home/dan/.screenlayout/laptop.sh && feh --bg-fill --no-xinerama --randomize ~/Media/images/wallpaper/*" )
  , ((m .|. shiftMask .|. mod1Mask, xK_s), spawn "/home/dan/.screenlayout/secondary.sh && feh --bg-fill --no-xinerama --randomize ~/Media/images/wallpaper/*" )
-- Arandr:1 ends here

-- [[file:../../README.org::*Cycle Recent Workspace][Cycle Recent Workspace:1]]
  , ((m .|. shiftMask, xK_Tab), cycleRecentWS [xK_Super_L] xK_Tab xK_BackSpace)
-- Cycle Recent Workspace:1 ends here

-- [[file:../../README.org::*Multiple Monitors][Multiple Monitors:1]]
  ] ++
  [((m .|. nilOrShift, key), screenWorkspace sc
          >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
       , (f, nilOrShift) <- [(W.view, 0), (W.shift, shiftMask)]]
-- Multiple Monitors:1 ends here

-- [[file:../../README.org::*Asthetics][Asthetics:1]]
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

-- [[file:../../README.org::*Float certain apps][Float certain apps:1]]
manageZoomHook =
  composeAll $
    [ (className =? zoomClassName) <&&> shouldFloat <$> title --> doFloat,
      (className =? zoomClassName) <&&> shouldSink <$> title --> doSink
    ]
  where
    zoomClassName = "zoom"
    tileTitles =
      [ "Zoom - Free Account", -- main window
        "Zoom - Licensed Account", -- main window
        "Zoom", -- meeting window on creation
        "Zoom Meeting" -- meeting window shortly after creation
      ]
    shouldFloat title = title `notElem` tileTitles
    shouldSink title = title `elem` tileTitles
    doSink = (ask >>= doF . W.sink) <+> doF W.swapDown

myHandleEventHook =
  mconcat
    [ dynamicTitle manageZoomHook,
      handleEventHook defaultConfig
    ]


myManageHook = composeAll [ appName =? "Open Files" --> doFloat,
                            className =? "Zenity" --> doFloat]
-- Float certain apps:1 ends here

-- [[file:../../README.org::*Screenshot][Screenshot:1]]
myScreenshot = do
  -- init takes care of the trailing newline character returned by date
  date <- init <$> runProcessWithInput "date" ["+%Y-%m-%d-%H:%M:%S"] []
  AL.launchApp myShellPrompt { defaultText = "~/screenshots/" ++ date ++ ".png"} "maim -s"
-- Screenshot:1 ends here

-- [[file:../../README.org::*Screenshot][Screenshot:2]]
myScreenshotClipboard :: X ()
myScreenshotClipboard = spawn  "maim -s | xclip -selection clipboard -t image/png"
-- Screenshot:2 ends here
