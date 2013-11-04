import XMonad
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.SimplestFloat
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Spacing 
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.EZConfig
import System.IO
import Control.Monad
import qualified XMonad.StackSet as W

defaultLayouts = tiled ||| Mirror tiled ||| Full  
  where  
      -- default tiling algorithm partitions the screen into two panes  
      tiled = spacing 3 $ Tall nmaster delta ratio  
   
      -- The default number of windows in the master pane  
      nmaster = 1  
   
      -- Default proportion of screen occupied by master pane  
      ratio = 2/3  
   
      -- Percent of screen to increment by when resizing panes  
      delta = 3/100  
   
 -- Define layout for specific workspaces  
nobordersLayout = noBorders $ Full  
myWorkspaces = ["1:web","2:shell","3:emacs","4:VM","5:long","6","7","8","9"]

myManageHook = composeAll
   [ className =? "Gimp"      --> viewShift "6"
   , className =? "Firefox" --> viewShift "1:web"
   , className =? "Emacs" --> viewShift "3:emacs"
   , className =? "Worker" --> viewShift "5:long"
   , className =? "trayer" --> doIgnore
   , (role =? "gimp-toolbox" <||> role =? "gimp-image-window") --> (ask >>= doF . W.sink)
   ]
  where role = stringProperty "WM_WINDOW_ROLE"
        viewShift = doF . liftM2 (.) W.greedyView W.shift
myLayout = onWorkspace "1:web" nobordersLayout $ onWorkspace "6" simplestFloat $ layoutHook defaultConfig

main = do
   init <- spawnPipe "/home/aitzol/.xmonad/autostart"
   xmproc <- spawnPipe "/usr/bin/xmobar /home/aitzol/.xmonad/xmobarrc"
   xmonad  $ defaultConfig
       { terminal = "urxvt"
       , modMask  = mod4Mask
       , borderWidth = 1
       , normalBorderColor = "#60A1AD"
       , focusedBorderColor = "#68e862"
       , workspaces = myWorkspaces
       , manageHook = myManageHook <+> manageHook defaultConfig
       , layoutHook = avoidStruts $ myLayout
       , logHook = dynamicLogWithPP $ xmobarPP
                       { ppOutput = hPutStrLn xmproc
                       , ppTitle = xmobarColor "green" "" . shorten 50
                       }
       , focusFollowsMouse = False
       } `additionalKeys`
       [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
       , ((mod4Mask .|. shiftMask, xK_F4), spawn "sudo shutdown -h now") 
       , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
       , ((0, xK_Print), spawn "scrot")
       , ((mod4Mask .|. controlMask, xK_f), spawn "firefox")
       , ((mod4Mask .|. controlMask, xK_e), spawn "emacs")
       , ((mod4Mask, xK_F1), spawn "mpc -h /tmp/mpdsocket toggle")
       , ((mod4Mask, xK_F2), spawn "mpc -h /tmp/mpdsocket prev")
       , ((mod4Mask, xK_F3), spawn "mpc -h /tmp/mpdsocket next")
       , ((mod4Mask, xK_F4), spawn "mpc -h /tmp/mpdsocket volume -2")      
       , ((mod4Mask, xK_F5), spawn "mpc -h /tmp/mpdsocket volume +2")      
       , ((mod4Mask, xK_F9), kill) -- to kill applications 
       ]
