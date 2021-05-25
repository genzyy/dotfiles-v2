Config { font = "xft:JetBrainsMono Nerd Font:size=9:weight=bold:antialias=true:hinting=true"
       , additionalFonts = [ "xft:Ubuntu Mono Ligaturized:pixelsize=10:antialias=true:hinting=true"
                           , "xft:JetBrainsMono Nerd Font:pixelsize=10:antialias=true:hinting=true"
                           , "xft:JetBrainsMono Nerd Font:pixelsize=15:antialias=true:hinting=true"
                           ]
       , borderColor = "#011627"
       , border = NoBorder
       , bgColor = "#011627"
       , fgColor = "#bbc2cf"
       , alpha = 190
       --, position = TopSize C 98 25
       , position = Static { xpos = 2 , ypos = 4, width = 1915, height = 25 }
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = True
       , hideOnStart = False
       , allDesktops = True
       , overrideRedirect = True
       , iconRoot = "/home/rishit/.xmonad/xpm/haskell_20"  -- default: "."
       , commands = [ 
                      Run WeatherX "KRNO"
                         [ ("clear",                    "  ")
                         , ("sunny",                    "  ")
                         , ("mostly clear",             "  ")
                         , ("mostly sunny",             "  ")
                         , ("partly sunny",             "  ")
                         , ("fair",                     "  ")
                         , ("cloudy",                   "  ")
                         , ("obscured",                 "  ")
                         , ("overcast",                 "  ")
                         , ("partly cloudy",            "  ")
                         , ("mostly cloudy",            "  ")
                         , ("rainy",                    "  ")
                         , ("showers",                  "  ")
                         , ("snowy",                    "流 ")
                         , ("snow",                     "流 ")
                         , ("heavy snow",               "流 ")
                         , ("light snow",               "流 ")
                         , ("moderate snow",            "流 ")
                         , ("patchy moderate snow",     "流 ")
                         , ("patchy heavy snow",        "流 ")
                         , ("considerable cloudiness",  "  ")]
                         ["-t", "<fn=3><skyConditionS></fn><tempF>°"
                         , "-L","20", "-H", "60", "--normal", "#51afef"
                         , "--high", "#ff6c6b", "--low", "#4db5bd"]
                         600
                    , Run Cpu ["-t", "<fn=3> </fn> <total>%"] 10
                    , Run Network "wlan1" ["-t", "<fn=3>歷 </fn> <fn=3> </fn><rx>K <fn=3> </fn><tx>K"
                                            , "-L","0","-H","10000000"
                                            , "--normal","#ecbe7b","--high","#ff6c6b"] 10
                    , Run Memory ["-t", "<fn=3> </fn><used>M (<usedratio>%)"] 20
                    , Run Com "kernel" [""] "" 36000
                    , Run Date "%a, %d %B | %H:%M" "date" 10
                    , Run StdinReader
                    , Run Battery        [ "--template" , "<acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "darkred"
                             , "--normal"   , "#98be65"
                             , "--high"     , "darkgreen"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"	, "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O"	, "<fc=#dAA520>Charging</fc>"
                                       -- charged status
                                       , "-i"	, "<fc=#006000>Charged</fc>"
                             ] 50
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " <icon=haskell_20.xpm/> <fc=#98be65><fn=3>  </fn></fc>  <fn=1>|</fn> %StdinReader% }\
                    \{ <fc=#ff6c6b>%cpu%</fc>  <fn=1>|</fn>  <fc=#98be65>%memory%</fc>  <fn=1>|</fn>  <fc=#ecbe7b>%wlan1%</fc>  <fn=1>|</fn>  <fc=#51afef>%KRNO%</fc>  <fn=1>|</fn>  <fc=#46d9ff><fn=3> </fn>%date%</fc> <fn=1>|</fn> <fc=#98be65><fn=3> </fn>%battery%  </fc>"
       }

