
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
import System.Exit
import qualified XMonad.StackSet as W
import System.IO (hPutStrLn)
-------------------------------------------------------------------
------                          DATA                         ------               
-------------------------------------------------------------------
import Data.Maybe (fromJust)
import Data.Maybe (isJust)
import qualified Data.Map as M
import Data.Monoid
import Data.Ratio

-------------------------------------------------------------------
------                        UTILLITIES                     ------               
-------------------------------------------------------------------
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor
import XMonad.Util.Run
import XMonad.Util.EZConfig
import Graphics.X11.ExtraTypes.XF86
-------------------------------------------------------------------
------                          HOOKS                        ------ 
-------------------------------------------------------------------
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten,pad, PP(..))
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))

-------------------------------------------------------------------
------                         LAYOUTS                       ------
-------------------------------------------------------------------
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed

-------------------------------------------------------------------
------                 LAYOUTS MODIFIERS                     ------
-------------------------------------------------------------------
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName -- This is a layout modifier that will show the workspace name
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.SimpleDecoration (shrinkText)
import XMonad.Layout.Tabbed (tabbed)
-------------------------------------------------------------------
------                         ACTIONS                       ------
-------------------------------------------------------------------
import XMonad.Actions.MouseResize
import XMonad.Actions.OnScreen
import XMonad.Actions.CopyWindow (kill1)
------------------ # End Import Section # -------------------------

-------------------------------------------------------------------

-------------------- # Variables Section # ------------------------    
myTerminal :: String
--myTerminal = "kitty"
myTerminal = "kitty"

myAltTerminal :: String
myAltTerminal = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

--myBorderWidth :: Dimension
myBorderWidth = 3

myNormalBorderColor :: String
myNormalBorderColor = "#38343f"
myFocusedBorderColor :: String
myFocusedBorderColor = "#38343f"

myModMask :: KeyMask
myModMask = mod4Mask

altMask :: KeyMask
altMask = mod1Mask

dmenu_con :: String
dmenu_con = "env LC_ALL=en_US.UTF-8 dmenu_run -b -fn 'JetBrainsMono Nerd Font-10' -nb '#011627' -nf '#ffffff' -sb '#51afef' -sf '#191c21'"

myWorkspaces = ["www", "dev", "home", "files", "music"]

-----------  make your workspace on xmobar clickable ------------
myWorkspacesIndices = M.fromList $ zipWith (,) myWorkspaces [1..] 

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspacesIndices
------------------------------------------------------------------

------------------ # End Variables Section # ----------------------   

-------------------------------------------------------------------
------                        LAYOUTS                        ------               
-------------------------------------------------------------------

myshowWNameTheme :: SWNConfig
myshowWNameTheme = def
    {
        swn_font    = "xft: Fira Code:bold:size=60:antialias=true:hinting=true"
    ,   swn_fade    = 0.7
    ,   swn_bgcolor = "#111111"
    ,   swn_color   = "#51afef"
    }

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myTabConfig = def { activeColor = "#556064"
                  , inactiveColor = "#2F3D44"
                  , urgentColor = "#FDF6E3"
                  , activeBorderColor = "#454948"
                  , inactiveBorderColor = "#454948"
                  , urgentBorderColor = "#268BD2"
                  , activeTextColor = "#80FFF9"
                  , inactiveTextColor = "#1ABC9C"
                  , urgentTextColor = "#1ABC9C"
                  , fontName = "xft:Fira Code:size=10:antialias=true"
                  }

myLayout = mouseResize $ windowArrange  $ mkToggle (NBFULL ?? FULL ?? EOT) $ avoidStruts  (
           tall         ||| 
           grid         |||
           mirror       |||
           threeCol     |||
           simplestFloat
           )            ||| 
           noBorders Full |||
           noBorders (tabbed shrinkText myTabConfig)

  where 
    tall     = renamed [Replace "Tall"] 
               $ smartBorders
               $ windowNavigation 
               $ subLayout [] (smartBorders Simplest)
               $ mySpacing 10 
               $ ResizableTall 1 (3/100) (1/2) [] 

    grid     = renamed [Replace "Grid"] 
               $ smartBorders
               $ windowNavigation 
               $ mySpacing 8
               $ Grid

    mirror   = renamed [Replace "Mirror"]
               $ smartBorders
               $ windowNavigation
               $ mySpacing 8
               $ Mirror (Tall 1 (3/100) (3/5))

    threeCol = renamed [Replace "ThreeCol"]
               $ smartBorders
               $ windowNavigation
               $ mySpacing 8
               $ ThreeCol 1 (3/100) (1/2)


windowCount :: X (Maybe String)
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

myManageHook = composeAll
    [ className =? "Slack"    --> doFloat
    , className =? "slack"            --> doFloat
    , className =? "notion"           --> doFloat
    , className =? "alacritty"        --> doFloat
    , className=? "Notion"            --> doFloat
    , className=? "dialog"            --> doCenterFloat
    , className=? "download"          --> doCenterFloat
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

myLogHook :: X ()
myLogHook = return () 
   where fadeAmount = 1.0

-------------------------------------------------------------------
------                     STARTUP HOOK                      ------               
-------------------------------------------------------------------

myStartupHook = do
    spawnOnce "xset b off"
    spawnOnce "~/.fehbg &"
    spawnOnce "env LC_ALL=en_US.UTF-8 /usr/bin/dunst"
    --spawnOnce "picom -b &"
    spawnOnce "picom -b --config ~/.config/picom.conf"
    spawnOnce "stalonetray --geometry 1x1-17+3 --max-geometry 10x1-17+5 --transparent --tint-color '#111111' --tint-level 255 --grow-gravity NE --icon-gravity NW --icon-size 20 --sticky --window-type dock --window-strut top --skip-taskbar"
    setDefaultCursor xC_left_ptr

-------------------------------------------------------------------
------                     KEY BINDINGS                      ------               
-------------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [
        ((modm,     xK_Return), spawn $ XMonad.terminal conf)
    ,   ((shiftMask .|. controlMask,    xK_Return), spawn myAltTerminal)
    ,   ((0,    xK_Print),spawn "flameshot gui")
    ,   ((modm,     xK_f), spawn "firefox")
    ,   ((modm,     xK_c), spawn "google-chrome-stable")
    ,   ((modm,     xK_d), spawn dmenu_con)
    ,   ((modm,     xK_space), sendMessage NextLayout)
    ,   ((modm .|. shiftMask,  xK_space), setLayout $ XMonad.layoutHook conf)
    ,   ((modm,     xK_r), refresh)
    --,   ((modm .|. shiftMask, xK_c), kill1)
    ,   ((modm,     xK_q), kill1)
    ,   ((modm,     xK_Tab), windows W.focusDown)
    ,   ((modm,     xK_j), windows W.focusDown)
    ,   ((modm,     xK_k), windows W.focusUp)
    ,   ((modm .|. shiftMask, xK_j), windows W.swapDown)
    ,   ((modm .|. shiftMask, xK_k), windows W.swapUp)
    ,   ((modm,     xK_h), sendMessage Shrink)
    ,   ((modm,     xK_l), sendMessage Expand)
    ,   ((modm .|. shiftMask, xK_h), sendMessage MirrorShrink)
    ,   ((modm .|. shiftMask, xK_l), sendMessage MirrorExpand)
    ,   ((modm,     xK_comma), sendMessage (IncMasterN 1))
    ,   ((modm,     xK_period), sendMessage (IncMasterN (-1)))
    ,   ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl -q s 5%+")
    ,   ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl -q s 5%-")
    ,   ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
    ,   ((0, xF86XK_AudioStop), spawn "playerctl stop")
    ,   ((0, xF86XK_AudioNext), spawn "playerctl next")
    ,   ((0, xF86XK_AudioPrev), spawn "playerctl previous")
    ,   ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    ,   ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    ,   ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    ,   ((modm .|. controlMask, xK_t), sendMessage ToggleStruts)
    ,   ((modm .|. shiftMask, xK_f), sendMessage $ Toggle NBFULL)
    ,   ((controlMask,     xK_q), io (exitWith ExitSuccess))
    ,   ((modm .|. shiftMask, xK_s), spawn "xmonad --recompile; xmonad --restart")
    ,   ((modm .|. shiftMask, xK_slash), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++
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

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [
        ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    ,   ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    ,   ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]

-------------------------------------------------------------------
------                        MAIN                           ------               
-------------------------------------------------------------------

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar -x 0 /home/rishit/.config/xmobar/xmobarrc1.hs"
    xmonad $ ewmh defaults {
        logHook = dynamicLogWithPP $ def
       { ppOutput = \x ->  hPutStrLn xmproc x 
                                , ppCurrent = xmobarColor              "#c678dd" "" . wrap "[" "]"             -- Current workspace
                                , ppVisible = xmobarColor              "#98be65" "" . wrap " " " "             -- Visible workspaces (f.e. on other monitors)
                                , ppHidden = xmobarColor               "#51afef" "" . wrap " " " "             -- Non-visible workspaces
                                , ppHiddenNoWindows = xmobarColor      "#ecbe7b" "" . wrap " " " "             -- Non-visible workspaces that also have no windows in them
                                , ppUrgent = xmobarColor               "#ff6c6b" "" . wrap "!" "!"             -- Urgent workspace
                                , ppLayout = xmobarColor               "#51afef" ""                            -- Current layout
                                , ppTitle = xmobarColor                "#c678dd" "" . shorten 50               -- Current window title
                                , ppSep =  "  <fn=1>|</fn>  "                                                  -- Separators
                                }

    }
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
