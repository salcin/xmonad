-- Import stuff
import XMonad
import XMonad.Config.Azerty
import qualified XMonad.StackSet as W
import qualified XMonad.StackSet as Z
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeys)
import System.Exit
import Graphics.X11.Xlib
import System.IO


-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.GridSelect
import XMonad.Actions.FloatKeys
import XMonad.Actions.Submap
import XMonad.Actions.DwmPromote
import XMonad.Actions.WindowBringer -- bring windows to yo and bring you to windows


-- utils
import XMonad.Util.Run
import qualified XMonad.Prompt 		as P
import XMonad.Prompt.Shell
import XMonad.Prompt
import XMonad.Prompt.AppendFile (appendFilePrompt)
import XMonad.Prompt.RunOrRaise
import XMonad.Util.NamedWindows (getName)
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid
import XMonad.Layout.ComboP
import XMonad.Layout.Column
import XMonad.Layout.Named
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed

-- Data.Ratio for IM layout
import Data.Ratio ((%))
import Data.List (isInfixOf)

import XMonad.Hooks.EwmhDesktops


-- Main --
main = do
    xmproc <- spawnPipe "xmobar --screen=0 ~/.xmonad/config/xmobarrc"

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig  {
          	  manageHook = myManageHook
        , layoutHook = myLayoutHook
		, borderWidth = myBorderWidth
		, normalBorderColor = myNormalBorderColor
		, focusedBorderColor = myFocusedBorderColor
		, keys = myKeys
        , modMask = myModMask
        , terminal = myTerminal
		, workspaces = myWorkspaces
                , focusFollowsMouse = True
                , logHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn xmproc , ppTitle = xmobarColor "#fbc55e" "" . shorten 50}
				, startupHook = ewmhDesktopsStartup >> setWMName "LG3D"
		}


-- hooks
-- automaticly switching app to workspace
myManageHook :: ManageHook
myManageHook = composeAll
                [ isFullscreen                  --> doFullFloat
        -- 1
        , className =? "Terminal"         --> doShift "1"
        , className =? "terminal"         --> doShift "1"

        -- 2
        , className =? "chromium"         --> doShift "2"
        , className =? "Icedove"         --> doShift "2"

        -- 3
        , className =? "Gvim"             --> doShift "3"
        , className =? "Geany"             --> doShift "3"

		-- 4
		, className =? "Kodi"         --> doShift "4"
		, className =? "vlc"         --> doShift "4"
		, className =? "mpv"	--> doShift "4"
        , (className =? "chromium" <&&> title =? "* YouTube – Chromium")         --> doShift "4"

        -- 5
        , className =? "Nemo"           --> doShift "5"

        -- 6
        , className =? "VirtualBox"           --> doShift "6"

		-- 7
		, className =? "Wine"	--> doShift "7"
		, className =? "Steam"	--> doShift "7"
		, className =? "hl2_linux"	--> doFullFloat
		, className =? "hl2_linux"	--> doShift "7"
		, className =? "dota2"	--> doFullFloat
		, className =? "dota2"	--> doShift "7"
		, title =? "/dev/ttyUSB1"	--> doShift "9"

		-- others
		, className =?  "Xmessage" 	--> doCenterFloat
		, className =? "Xfce4-notifyd" --> doIgnore
	    , className =? "stalonetray" --> doIgnore
        , className =? "Gimp"           --> doShift "9"
		, className =? "rdesktop"	--> doShift "6"
		, className =? "Spotify"	--> doShift "10"
                , fmap ("libreoffice"  `isInfixOf`) className --> doShift "5"
		, className =? "MPlayer"	--> (ask >>= doF . W.sink)
		, manageDocks
	    	, scratchpadManageHook (W.RationalRect 0.125 0.25 0.75 0.5)
                ]

--    , className =? "Xfce4-notifyd"    --> doF W.focusDown <+> doF copyToAll
--    , className =? "File Operation Progress"            --> doFloat
--    , className =? "Enregistrer le fichier"         --> doFloat



-- scratchpads
-- scratchpads = [ NS "gvim" "gvim -S ~/.vim/sessions/Session.vim" (className =? "Gvim") (customFloating $ W.RationalRect (0) (0) (0) (0)) ]


--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }
---- Looks --
---- bar
customPP :: PP
customPP = defaultPP {
     			    ppHidden = xmobarColor "#6f6e6e" ""
			      , ppCurrent = xmobarColor "#fbc55e" "" . wrap "[" "]"
			      , ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
                  , ppLayout = xmobarColor "#FF0000" ""
                  , ppTitle = xmobarColor "#6f6e6e" "" . shorten 80
                  --, ppSep = "<fc=#0033FF> | </fc>"
                     }

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig
    {
	-- font  = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u"
	font = "xft:Sans-8:bold"
	,fgColor = "#fbc55e"
	, bgColor = "#333333"
	, bgHLight    = "#000000"
	, fgHLight    = "#FF0000"
	, position = Top
    , historySize = 512
    , showCompletionOnTab = True
    , historyFilter = deleteConsecutive
    }



--LayoutHook
myLayoutHook  =  onWorkspace "1" imLayout  $ onWorkspace "6" fullL $ onWorkspace "7" fullL  $ standardLayouts
   where
	standardLayouts =   avoidStruts  $ (noBorders simpleTabbed ||| tiled |||  reflectTiled ||| Mirror tiled ||| Grid) 

    --Layouts
	tiled     = smartBorders (ResizableTall 1 (2/100) (1/2) [])
        reflectTiled = (reflectHoriz tiled)
	full 	  = noBorders simpleTabbed

    --Im Layout
	--Show pidgin tiled left and skype right
        imLayout = avoidStruts $ smartBorders $ withIM ratio pidginRoster $ reflectHoriz $ withIM skypeRatio skypeRoster (tiled ||| reflectTiled ||| Grid) where
                chatLayout      = Grid
	        ratio = (1%9)
                skypeRatio = (1%8)
                pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
                skypeRoster  = (ClassName "Skype")     `And`
                               (Not (Title "Options")) `And`
                                              (Not (Role "Chats"))    `And`
                                                             (Not (Role "CallWindowForm"))
	--Weblayout
	webL      = avoidStruts $  simpleTabbed ||| tiled ||| reflectHoriz tiled

    --VirtualLayout
        fullL = avoidStruts $ simpleTabbed



-------------------------------------------------------------------------------
---- Terminal --
myTerminal :: String
myTerminal = "terminator"


-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
myModMask :: KeyMask
myModMask = mod4Mask
myaltMask = mod1Mask

-- borders
myBorderWidth :: Dimension
myBorderWidth = 2
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#393833"
myFocusedBorderColor = "#256373"

--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1", "2", "3", "4", "5", "6" ,"7", "8", "9","10"]


-- keys
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- killing programs
    [ ((modMask, xK_c ), kill)
    --((modMask, xK_Return), spawn $ XMonad.terminal conf)

    -- swap the focused window and the master window
    , ((modMask, xK_Return ), dwmpromote )
    --, ((modMask .|. shiftMask, xK_c ), kill)

    -- opening program launcher / search engine
    ,((modMask , xK_p), shellPrompt myXPConfig)

    -- GridSelect
    , ((modMask, xK_g), goToSelected defaultGSConfig)

    -- layouts
    , ((modMask, xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_b ), sendMessage ToggleStruts)

    -- floating layer stuff
    --, ((modMask, xK_t ), withFocused $ windows . W.sink)
    , ((modMask, xK_s ), runOrRaise "nemo" (className =? "Nemo"))

    -- refresh'
    , ((modMask, xK_n ), refresh)

    -- focus
	-- switch on the last focus workspace
    , ((modMask, xK_a ), toggleWS )

    , ((modMask, xK_Tab ), windows W.focusDown)
    , ((modMask, xK_j ), windows W.focusDown)
    , ((modMask, xK_k ), windows W.focusUp)
    , ((modMask, xK_m ), windows W.focusMaster)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j ), windows W.swapDown )
    , ((modMask .|. shiftMask, xK_k ), windows W.swapUp )

    -- increase or decrease number of windows in the master area
    , ((modMask , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask, xK_h ), sendMessage Shrink)
    , ((modMask, xK_l ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l ), sendMessage MirrorExpand)

    , ((modMask, xK_f ), gotoMenu) 	  -- search in the softwares running
    , ((modMask, xK_F ), bringMenu)	  -- search and bring to window

    -- scratchpad
    --, ((modMask,  xK_f ),  namedScratchpadAction scratchpads "gvim")
    , ((modMask,  xK_x ),  safeSpawn "xscreensaver-command" ["--lock"] )
    --, ((myaltMask .|. shiftMask,  xK_x ),  safeSpawn "xdotool" ["key", "super+shift+8"] )
    --xdotool key super+shift+6

    --Spotify
    --, ((modMask , xK_a ), safeSpawn "dbus-send" ["--print-reply"," --dest=org.mpris.MediaPlayer2.spotify", "/org/mpris/MediaPlayer2", "org.mpris.MediaPlayer2.Player.Previous"] )
    --, ((modMask, xK_s ), safeSpawn "dbus-send" ["--print-reply", "--dest=org.mpris.MediaPlayer2.spotify", "/org/mpris/MediaPlayer2", "org.mpris.MediaPlayer2.Player.PlayPause"] )
    --, ((0, xF86XK_AudioPlay ), safeSpawn "dbus-send" ["--print-reply", "--dest=org.mpris.MediaPlayer2.spotify", "/org/mpris/MediaPlayer2", "org.mpris.MediaPlayer2.Player.PlayPause"] )
    --, ((modMask, xK_d ), safeSpawn "dbus-send" ["--print-reply", "--dest=org.mpris.MediaPlayer2.spotify", "/org/mpris/MediaPlayer2", "org.mpris.MediaPlayer2.Player.Next"] )

    --Launching programs
    -- , ((0, xF86XK_Favorites ), safeSpawn "")
    -- , ((0, xF86XK_Mail ), runOrRaise "thunderbird" (className =? "Thunderbird"))
    -- , ((0, xF86XK_Messenger ), runOrRaise "pidgin" (className =? "Pidgin"))

    --, ((0, 0x1008ff18 ), runOrRaise "aurora" (className =? "Aurora"))
    --, ((0, xF86XK_Calculator	), safeSpawn "gnome-calculator" [])
    --, ((0, xF86XK_Display	), spawn "bash /home/jelle/bin/xrandr-laptop")

    , ((modMask,  xK_y ), safeSpawn "mpv" ["--shuffle", "/media/nas/TMP/Arte+"]) -- arte read random file
    --
    , ((modMask,  xK_i ), safeSpawn "gmusicbrowser" ["-cmd", "NextSong"])    -- song next
    , ((modMask,  xK_o ), safeSpawn "gmusicbrowser" ["-cmd", "PrevSong"])    -- song prev
    , ((modMask,  xK_u ), safeSpawn "gmusicbrowser" ["-cmd", "PlayPause"]) -- song pause/play

    -- quit, or restart
    -- , ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
    , ((modMask .|. shiftMask, xK_a ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[e,w,r] %! switch to twinview screen 1/2/3
    -- mod-shift-[e,w] %! move window to screen 1/2/3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_z, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
