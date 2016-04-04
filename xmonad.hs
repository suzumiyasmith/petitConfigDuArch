import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders

import XMonad.Wallpaper
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops

main = do
  setRandomWallpaper ["/home/suzumiya/Wallpaper"]
  xmonad mySimpleConfig


mySimpleConfig = desktopConfig
  { terminal = "terminator"
  , modMask  = mod4Mask
  , layoutHook = smartBorders $ layoutHook desktopConfig
  , manageHook = myManageHooks
  , handleEventHook = fullscreenEventHook
  }

myManageHooks = composeAll [ isFullscreen --> doFullFloat]
