-- #########################################
--  # personal xmonad config
--  # genzyy
--  # github: https://www.github.com/genzyy
-- #########################################


--------------------- # Import Module Section # -------------------- 

--------------------------------------------------------------------
--                             CORE                               --
--------------------------------------------------------------------

import XMonad
-- the basic module for xmonad tiling.
import System.Exit
-- exit codes that a program can return.
import qualified Xmonad.StackSet as W
-- managing windows in a particular workspace.
import System.IO (hPutStrLn)
-- haskell standard I/O library (the thing inside the bracket is its instance).

-------------------------------------------------------------------
------                          DATA                         ------               
-------------------------------------------------------------------

import Data.Maybe (fromJust)
import Data.Maybe (isJust)
-- to use maybe type associations and operations.
import qualified Data.Map as M
-- map data structure.
import Data.Monoid
-- A type a is a Monoid if it provides an associative function (<>) 
-- that lets you combine any two values of type a into one,
-- and a neutral element (mempty) such that a <> mempty == mempty <> a == a.
import Data.Ratio
-- Ratio datatype haskell.

-------------------------------------------------------------------
------                        UTILLITIES                     ------               
-------------------------------------------------------------------

import XMonad.Util.SpawnOnce
-- SpawnOnce used for starting applications after boot.
import XMonad.Util.Cursor
-- the default xmonad cursor is a cross icon so to change to left_ptr we use this.
import XMonad.Util.Run
-- provides commands to run as external commands.
import XMonad.Util.EZConfig
-- helper function to override default config and an emacs type keyboard config.
import XMonad.Util.ExtraTypes.XF86
-- support multimedia keyboard keys.

-------------------------------------------------------------------
------                          HOOKS                        ------ 
-------------------------------------------------------------------

import XMonad.Hooks.ManageDocks
-- automatically manages xmobar, gnome-panel or few more panel apps.
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten,pad, PP(..))
-- to pass the output of commands to external status bar apps (xmobar).
import XMonad.Hooks.SetWMName
-- set a custom name for the current window manager.
import XMonad.Hooks.ManageHelpers
-- helper functions for defining rules for certain apps & their behaviour in our wm.
import XMonad.Hooks.EwmhDesktops
-- allows us to click on the panel apps (xmobar) & panel modules.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))

-------------------------------------------------------------------
------                         LAYOUTS                       ------
-------------------------------------------------------------------

import XMonad.Layout.Grid
-- alligns all the windows in a grid style.
import XMonad.Layout.Gaps
-- for gaps around the edges & live configuring the outer/inner gaps.
import XMonad.Layout.NoBorders
-- the given layout is displayed without any borders.
import XMonad.Layout.ThreeColumns
-- a layout with three columns.
import XMonad.Layout.ResizableTile
-- tiling with on-the-go resizability.
import XMonad.Layout.SimplestFloat
-- simple floating windows without any decorations.

-------------------------------------------------------------------
------                 LAYOUTS MODIFIERS                     ------
-------------------------------------------------------------------

import XMonad.Layout.LayoutModifier
-- easy modifying the base layouts according to the taste.
import XMonad.Layout.Spacing
-- for spacing between the windows.
import XMonad.Layout.WindowArranger
-- resize window with your keyboard.
import XMonad.Layout.Renamed
-- modify layout on a (hopefully) flexible way.
import XMonad.Layout.ShowWName
-- show the current workspace name.
import XMonad.Layout.WindowNavigation
-- easy navigation of workspace using keyboard.
import XMonad.Layout.Simplest
-- simplest layout
import XMonad.Layout.SubLayouts
-- nested layout
import XMonad.Layout.MultiToggle
-- to apply specific layout rules to an app in the current layout maybe to go fullscreen like that.
import XMonad.Layout.MultiToggle.Instances
 -- instances to use with MultiToggle.

 -------------------------------------------------------------------
------                         ACTIONS                       ------
-------------------------------------------------------------------

import XMonad.Actions.MouseResize
-- resize windows with mouse.
import XMonad.Actions.OnScreen
-- control workspaces while in xinerama mode.
import XMonad.Actions.CopyWindow (kill1)
-- to kill a process or a window with a keybind.
------------------ # End Import Section # -------------------------
-------------------------------------------------------------------

-------------------- # Variables Section # ------------------------   

myTerminal :: String
myTerminal = "kitty"
-- the default terminal.

myAltTerminal :: String
myAltTerminal = "alacritty"
-- my alternate terminal.

myFocusFollowsMouse :: Bool
-- to focus or not when hovering on a window (set to false).
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
-- to focus or not when clicking on a window (set to true).
myClickJustFocuses = True

myBorderWidth :: Integer
-- set a width for the border aorund the windows.
myBorderWidth = 3

myNormalBorderColor :: String
-- normal/unfocused window border colour.
myNormalBorderColor = "#a89884"
myFocusedBorderColor :: String
-- focused window border colour.
myFocusedBorderColor = "#b8bb26"

myModMask :: KeyMask
-- set your super key (set to windows key).
myModMask = mod4Mask

altMask :: KeyMask
-- set an alternate key as alt key.
altMask = mod1Mask

myWorkspaces = ["www", "dev", "home", "files", "music"]
-- set your custom workspaces.
-- myWorkspaces = ["異","二","三","四","五","六","七","八","九"]

myWorkspacesIndices = M.fromList $ zipWith (,) myWorkspaces [1..] 
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspacesIndices
-- make the workspace module clickable on xmobar.

myshowWNameTheme :: SWNConfig
-- defining custom colours & fonts for xmonad workspace change indicator.
-- show wm name.
myshowWNameTheme = def
    {
        swn_font    = "xft: JetBrainsMono Nerd Font:bold:size=60:antialias=true:hinting=true"
    ,   swn_fade    = 0.7
    ,   swn_bgcolor = "#191c21"
    ,   swn_color   = "#23a611"
    }

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
-- configuring spaces in between the windows.
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- renaming window layouts and reordering the lists of available layouts.
myLayout = mouseResize $ windowArrange  $ mkToggle (NBFULL ?? FULL ?? EOT) $ avoidStruts  (
           monocle      |||
           tall         ||| 
           grid         |||
           mirror       |||
           threeCol 
           )            ||| 
           noBorders Full

  where 
    tall     = renamed [Replace "Tall"] 
               $ windowNavigation 
               $ subLayout [] (smartBorders Simplest)
               $ mySpacing 8 
               $ ResizableTall 1 (3/100) (1/2) [] 

    grid     = renamed [Replace "Grid"] 
               $ windowNavigation 
               $ mySpacing 8
               $ Grid

    mirror   = renamed [Replace "Mirror"]
               $ windowNavigation
               $ mySpacing 8
               $ Mirror (Tall 1 (3/100) (3/5))

    threeCol = renamed [Replace "ThreeCol"]
               $ windowNavigation
               $ mySpacing 8
               $ ThreeCol 1 (3/100) (1/2)

     monocle = renamed [Replace "monocle"]
               $ smartBorders
               $ subLayout [] (smartBorders Simplest)
               $ Full


windowCount :: X (Maybe String)
-- window count and custom layout.
windowCount =
  gets
    $ Just
    . show
    . length
    . W.integrate'
    . W.stack
    . W.workspace
    . W.current
    . windowset

-------------------------------------------------------------------
------                   WINDOW RULES                        ------               
-------------------------------------------------------------------
-- defining custom rules for certain apps that I use.
myManageHook = composeAll
    [
        className =? "Slack-desktop"    --> doFloat
    ,   className =? "Notion-desktop"   --> doFloat
    ,   className =? "alacritty"        --> doFloat
    ]
       <+>composeOne
    [
       className =? "Pcmanfm"  -?> doRectFloat $ (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2) ) 
    ]

-------------------------------------------------------------------
------                   EVENT HANDLING                      ------               
-------------------------------------------------------------------

-------------------------------------------------------------------
------                  STATUS BARS & LOGGING                ------               
-------------------------------------------------------------------

-- fadeInactiveLogHook fadeAmount
myLogHook :: X ()
myLogHook = return () 
   where fadeAmount = 1.0

-------------------------------------------------------------------
------                     STARTUP HOOK                      ------               
-------------------------------------------------------------------

-- this array contains the apps that are started after the boot.
myStartupHook = do
    spawnOnce "./fehbg &"
    spawnOnce "picom &"
    spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype request  --transparent true --alpha 55 --tint 0x000000  --height 22 --monitor 0 --iconspacing 2 &"

-------------------------------------------------------------------
------                     KEY BINDINGS                      ------               
-------------------------------------------------------------------
-- custom keybindings (with keyboard only).
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [   -- open default terminal.
        ((modm,     xK_Return), spawn $ XMonad.terminal conf)
        -- open alternate terminal.
    ,   ((shiftMask .|. controlMask,    xK_Return), spawn myAltTerminal)
    -- take screenshot.
    ,   ((0,    xK_Print),spawn "scrot 'scrot-%Y-%m-%d_$wx$h.png' -s -e   'mv $f ~/screenshoots'")
    -- open firefox.
    ,   ((modm,     xK_f), spawn "firefox")
    -- open chrome LOL.
    ,   ((modm,     xK_c), spawn "google-chrome-stable")
    -- open dmenu.
    ,   ((modm,     xK_d), spawn "dmenu_run -h 24")
    -- change to next available/included layout.
    ,   ((modm,     xK_space), sendMessage NextLayout)
    -- to change the layout with all the current windows.
    ,   ((modm .|. shiftMask,  xK_space), setLayout $ XMonad.layoutHook conf)
    ,   ((modm,     xK_r), refresh)
    -- focus to the window below the current window.
    ,   ((modm,     xK_Tab), windows W.focusDown)
    -- focus to the window below the current window.
    ,   ((modm,     xK_j), windows W.focusDown)
    -- focus to the window above the current window.
    ,   ((modm,     xK_k), windows W.focusUp)
    -- swap the window to the window below it.
    ,   ((modm .|. shiftMask, xK_j), windows W.swapDown)
    -- swap the window to the window above it.
    ,   ((modm .|. shiftMask, xK_k), windows W.swapUp)
    -- shrink means to make the focused window shrink.
    ,   ((modm,     xK_h), sendMessage Shrink)
    -- expand means to make the focused window expand.
    ,   ((modm,     xK_l), sendMessage Expand)
    --  mirrorshrink means to make the focused window shrink in opp direction.
    ,   ((modm .|. shiftMask, xK_h), sendMessage MirrorShrink)
    --  mirrorexpand means to make the focused window expand in opp direction.
    ,   ((modm .|. shiftMask, xK_l), sendMessage MirrorExpand)
    ,   ((modm,     xK_comma), sendMessage (IncMasterN 1))
    ,   ((modm,     xK_period), sendMessage (IncMasterN (-1)))
    -- multimedia play-pause key
    ,   ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
    -- multimedia stop key
    ,   ((0, xF86XK_AudioStop), spawn "playerctl stop")
    -- multimedia next key
    ,   ((0, xF86XK_AudioNext), spawn "playerctl next")
    -- multimedia previous key
    ,   ((0, xF86XK_AudioPrev), spawn "playerctl previous")
    -- volume mute key
    ,   ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    -- volume up key
    ,   ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    -- volume down key
    ,   ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    ,   ((modm .|. controlMask, xK_t), sendMessage ToggleStruts)
    ,   ((modm .|. shiftMask, xK_f), sendMessage $ Toggle NBFULL)
    -- close xmonad.
    ,   ((modm,     xK_q), io (exitWith ExitSuccess))
    -- recompile xmonad.
    ,   ((modm .|. shiftMask, xK_r), spawn "xmonad --recompile; xmonad --restart")
    -- xmonad error message.
    ,   ((modm .|. shiftMask, xK_slash), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++
    -- change workspaces using keybinds as super key + num 1,2,3...
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
        [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-------------------------------------------------------------------
------                   MOUSE BINDINGS                      ------               
-------------------------------------------------------------------

-- mouse bindings for resize, relocate windows.
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [
        ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    ,   ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    ,   ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

-------------------------------------------------------------------
------                        MAIN                           ------               
-------------------------------------------------------------------
-- main function to use everything we have done so far.
main :: IO ()
main = do
    -- define the xmobarrc to use with xmobar.
    xmproc <- spawnPipe "xmobar -x 0 /home/rishit/.config/xmobar/xmobarrc"
    xmonad $ ewmh defaults {
        logHook = dynamicLogWithPP $ def
        {
            ppOutput = \x ->  hPutStrLn xmproc x
        ,   ppCurrent = xmobarColor "#cbe500" "#078202"  . wrap " "  " " -- "#71fe00" 
        ,   ppVisible = xmobarColor "#2ba402" "" .  clickable
        ,   ppHidden  = xmobarColor "#498236" "" . wrap "" "*" . clickable
        ,   ppTitle   = xmobarColor "#22d964" "" . shorten 50
        ,   ppSep     =  "<fc=#666666> | </fc>"
        ,   ppHiddenNoWindows = xmobarColor "#373b41" "" . wrap "|" " " 
        ,   ppExtras  = [windowCount] 
        ,   ppOrder   = \(ws:l:t:ex) -> [ws, l]++ex++[t]
        }
    }

-- variable we declared above are now being used.
defaults = def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = showWName' myshowWNameTheme $ myLayout,
        manageHook         = myManageHook <+> manageDocks,
        handleEventHook    = docksEventHook,
        logHook            = myLogHook ,
        startupHook        = myStartupHook
}

-- help incase xmonad.hs doesnt compile and then xmonad reverts back to default config.
help :: String
help = unlines ["The default modifier key is 'alt'. Default Keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]