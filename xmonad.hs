{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}

import           XMonad

-- Deal with docks and fullScreen
import           XMonad.Hooks.EwmhDesktops       (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks        (ToggleStruts (..),
                                                  avoidStruts, docks)
import           XMonad.Layout.NoBorders         (smartBorders)

-- wallpaper
import           XMonad.Hooks.WallpaperSetter


-- actions
import           XMonad.Actions.CycleWS
import           XMonad.Actions.GridSelect
import           XMonad.Hooks.ScreenCorners

-- customised layout
import           XMonad.Layout.DecorationMadness
import           XMonad.Layout.Tabbed
import           XMonad.Util.Themes

import           XMonad.Actions.MouseResize
import           XMonad.Layout.Decoration
import           XMonad.Layout.DwmStyle
import           XMonad.Layout.SimpleDecoration
import           XMonad.Layout.TabBarDecoration

import           XMonad.Layout.Accordion
import           XMonad.Layout.Circle
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.WindowArranger

-- helpers
import           XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet                 as W
import           XMonad.Util.EZConfig

-- control
import           Control.Arrow                   hiding ((|||))
import           Data.Monoid

------------
-- Basics --
------------
myTerminal = "terminator"

myModMask = mod4Mask -- super(win)

myBorderColor = "#7FBC71" -- lightGreen
myNormalBorderColor = "#eeeeee" -- white
myBorderWidth = 4

---------------
-- Wallpaper --
---------------
wallpaperHook =
  wallpaperSetter $ WallpaperConf "/" $ WallpaperList $
    (id &&& (const $ WallpaperDir wallpaperDir)) <$> myWorkspaces

wallpaperDir = "/home/suzumiya/.local/share/wallpapers/resized/"

myWorkspaces = show <$> [1..9 :: Int]

-----------------
-- KeyBindings --
-----------------
myKeys =
  [ ("M-f"   , spawn "firefox")  -- Firefox
  , ("M-x"   , spawn "xfce4-taskmanager") -- tasX
  , ("M-z"   , spawn "hyper") -- hyperterm
  , ("M-p"   , spawn "xfce4-popup-whiskermenu") -- windows menu
  , ("M-S-p" , screenshot "")
  , ("M-b"   , hideDocks)
  ]

screenshot opts = spawn $ unwords
  [ "sleep 0.2;"
  , "scrot "
  , opts
  , "-e 'xdg-open $f'"
  , "$HOME/Pictures/screenshot-%Y-%m-%d-%H%M%S.png"
  ]

hideDocks = sendMessage ToggleStruts

-----------------
-- LayoutHooks --
-----------------
myLayoutHook = smartBorders . avoidStruts
  $ myNormalLayout 2
  ||| myMadLayout 7
  ||| myTabbedLayout 10
  ||| Full

myNormalLayout =  tallDwmStyle shrinkText . themeOrder

myTabbedLayout = tabbed shrinkText . themeOrder

myMadLayout = mirrorTallDeco shrinkText . themeOrder

themeOrder :: Int -> Theme
themeOrder i = theme $ listOfThemes !! i -- length listOfThemes == 15

-------------------
-- WindowActions --
-------------------
myHandleEventHook = handleEventHook def
  <> fullscreenEventHook
  <> screenCornerEventHook

myStartupHook = do
  addScreenCorner SCUpperRight $ goToSelected def { gs_cellwidth = 250}
  addScreenCorners [ (SCLowerRight, nextWS) , (SCLowerLeft,  prevWS) ]

-----------------
-- ManageHooks --
-----------------
myManageHook :: ManageHook
myManageHook = mconcat
  [ isFullscreen --> doFullFloat
  , isDialog --> doCenterFloat
  , fmap not isDialog --> doF avoidMaster
  , appManageHooks
  , manageHook def
  ]

appManageHooks :: ManageHook
appManageHooks = mconcat [ matchAny v --> a | (v,a) <- myActions]
  where
    myActions =
      [ ("Xfrun4"                         , doFloat)
      , ("Xfce4-notifyd"                  , doIgnore)
      , ("MPlayer"                        , doFloat)
      , ("mpv"                            , doFloat)
      , ("Firefox"                        , doShift "2")
      , ("Atom"                           , doShift "3")
      , ("Oracle VM VirtualBox Manager"   , doShift "8")
      , ("VirtualBox"                     , doShift "9")
      , ("gimp-image-window"              , ask >>= doF . W.sink)
      , ("gimp-toolbox"                   , ask >>= doF . W.sink)
      , ("gimp-dock"                      , ask >>= doF . W.sink)
      , ("gimp-image-new"                 , doFloat)
      , ("gimp-toolbox-color-dialog"      , doFloat)
      , ("gimp-layer-new"                 , doFloat)
      , ("gimp-vectors-edit"              , doFloat)
      , ("gimp-levels-tool"               , doFloat)
      , ("preferences"                    , doFloat)
      , ("gimp-keyboard-shortcuts-dialog" , doFloat)
      , ("gimp-modules"                   , doFloat)
      , ("unit-editor"                    , doFloat)
      , ("screenshot"                     , doFloat)
      , ("gimp-message-dialog"            , doFloat)
      , ("gimp-tip-of-the-day"            , doFloat)
      , ("plugin-browser"                 , doFloat)
      , ("procedure-browser"              , doFloat)
      , ("gimp-display-filters"           , doFloat)
      , ("gimp-color-selector"            , doFloat)
      , ("gimp-file-open-location"        , doFloat)
      , ("gimp-color-balance-tool"        , doFloat)
      , ("gimp-hue-saturation-tool"       , doFloat)
      , ("gimp-colorize-tool"             , doFloat)
      , ("gimp-brightness-contrast-tool"  , doFloat)
      , ("gimp-threshold-tool"            , doFloat)
      , ("gimp-curves-tool"               , doFloat)
      , ("gimp-posterize-tool"            , doFloat)
      , ("gimp-desaturate-tool"           , doFloat)
      , ("gimp-scale-tool"                , doFloat)
      , ("gimp-shear-tool"                , doFloat)
      , ("gimp-perspective-tool"          , doFloat)
      , ("gimp-rotate-tool"               , doFloat)
      , ("gimp-open-location"             , doFloat)
      , ("gimp-file-open"                 , doFloat)
      , ("animation-playbac"              , doFloat)
      , ("gimp-file-save"                 , doFloat)
      , ("file-jpeg"                      , doFloat)
      ]

-- Match a string against any one of a window's class, title, name or
-- role.
matchAny :: String -> Query Bool
matchAny x = foldr ((<||>) . (=? x)) (return False) [className, title, name, role]

-- Match against @WM_NAME@.
name :: Query String
name = stringProperty "WM_CLASS"

-- Match against @WM_WINDOW_ROLE@.
role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

-- Helpers --
-- avoidMaster:  Avoid the master window, but otherwise manage new windows normally
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) -> W.Stack t [r] rs
    _                   -> c


----------
-- Main --
----------
main = xmonad myXfceConfig

myXfceConfig = docks $ ewmh def
  { terminal = myTerminal
  , modMask  = myModMask
  , focusedBorderColor = myBorderColor
  , normalBorderColor = myNormalBorderColor
  , borderWidth = myBorderWidth
  , manageHook = myManageHook
  , layoutHook = myLayoutHook
  , logHook = wallpaperHook
  , workspaces = myWorkspaces
  , handleEventHook = myHandleEventHook
  , startupHook = myStartupHook
  } `additionalKeysP` myKeys
