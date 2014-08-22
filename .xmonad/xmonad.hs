{- xmonad.hs
 - Author: Jelle van der Waa ( jelly12gen )
 -}

-- Import stuff
import XMonad
import qualified XMonad as XM
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeys)
import System.Exit
import Graphics.X11.Xlib
import System.IO
import qualified XMonad.StackSet as W


-- actions
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import qualified XMonad.Actions.Search as S
import XMonad.Actions.Search
import qualified XMonad.Actions.Submap as SM
import XMonad.Actions.GridSelect

-- utils
import XMonad.Util.Scratchpad (scratchpadSpawnAction, scratchpadManageHook, scratchpadFilterOutWorkspace)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Prompt 		as P
import XMonad.Prompt.Shell
import XMonad.Prompt


-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Grid
       
import XMonad.Util.EZConfig

-- Data.Ratio for IM layout
import Data.Ratio ((%))

scratchpads =
  [ NS "htop" "urxvt -e htop" (title =? "htop") (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
  , NS "notes" "emacs --title emacs-notes ~/workspace/haskell/henge/docs/main.org" (title =? "emacs-notes") (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
  , NS "xmonad-config" "emacs --title emacs-xmonad ~/.xmonad/xmonad.hs" (title =? "emacs-xmonad") (customFloating $ W.RationalRect 0.1 0.1 0.8 0.8)
  , NS "buildTerm" "urxvt -title buildTerm"  (title =? "buildTerm") (customFloating $ W.RationalRect 0.25 0.25 0.5 0.5) 
  ] -- where role = stringProperty "WM_WINDOW_ROLE"

-- Main --
main = do
        xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"  -- start xmobar
        --xmproc <- spawnPipe "xmobar"
    	xmonad 	$ withUrgencyHook NoUrgencyHook $ defaultConfig
        	{ manageHook = namedScratchpadManageHook scratchpads XM.<+> myManageHook
        	, layoutHook = myLayoutHook
		, borderWidth = myBorderWidth
		, normalBorderColor = myNormalBorderColor
		, focusedBorderColor = myFocusedBorderColor
		, keys = myKeys
		, logHook = myLogHook xmproc
        	, modMask = myModMask
        	, terminal = myTerminal
		, workspaces = myWorkspaces
                , focusFollowsMouse = False
		}
                `additionalKeysP`
                [ ("S-\\ h", namedScratchpadAction scratchpads "htop")
                , ("S-\\ n", namedScratchpadAction scratchpads "notes")
                , ("S-\\ c", namedScratchpadAction scratchpads "buildTerm")
                , ("S-\\ x", namedScratchpadAction scratchpads "xmonad-config")
                ]


-- hooks
-- automaticly switching app to workspace
myManageHook :: ManageHook
myManageHook =  
                scratchpadManageHook (W.RationalRect 0.25 0.375 0.5 0.35) XM.<+> ( XM.composeAll . concat $
                [[isFullscreen                  XM.--> doFullFloat
		]]
                        )  XM.<+> manageDocks XM.<+> composeAll
                [ (role =? "gimp-toolbox" <||> role =? "gimp-image-window") --> (ask >>= doF . W.sink)
                ]
        where
            role = stringProperty "WM_WINDOW_ROLE"


--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP { ppOutput = hPutStrLn h }



---- Looks --
---- bar
customPP :: PP
customPP = defaultPP {
     			    ppHidden = xmobarColor "#00FF00" ""
			  , ppCurrent = xmobarColor "#FF0000" "" . wrap "[" "]"
			  , ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
                          , ppLayout = xmobarColor "#FF0000" ""
                          , ppTitle = xmobarColor "#00FF00" "" . shorten 80
                          , ppSep = "<fc=#0033FF> | </fc>"
                     }

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = defaultXPConfig
    {
	font  = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-u"
	,fgColor = "#00FFFF"
	, bgColor = "#000000"
	, bgHLight    = "#000000"
	, fgHLight    = "#FF0000"
	, position = Top
    }

--- My Theme For Tabbed layout
myTheme = defaultTheme { decoHeight = 16
                        , activeColor = "#a6c292"
                        , activeBorderColor = "#a6c292"
                        , activeTextColor = "#000000"
                        , inactiveBorderColor = "#000000"
                        }

--LayoutHook
myLayoutHook  =  avoidStruts  $ (Grid ||| Full ||| tiled)
   where
    tiled = Tall master delta ratio
    master = 1
    ratio = 1/2
    delta = 3/100


-------------------------------------------------------------------------------
---- Terminal --
myTerminal :: String
myTerminal = "urxvt"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
myModMask :: KeyMask
myModMask = mod4Mask



-- borders
myBorderWidth :: Dimension
myBorderWidth = 2
--
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "#333333"
myFocusedBorderColor = "#FF0000"
--


--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:chat", "2:web", "3:code", "4:gimp", "5:doc", "6:vbox" ,"7:games", "8:vid", "9:gimp"]
--

-- Switch to the "web" workspace
viewWeb = windows (W.greedyView "2:web")                           -- (0,0a)
--

ssh = "ssh -i /path/to/file/.key usere@domain "
-- keys
myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- killing programs
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask, xK_c ), kill)

    -- opening program launcher / search engine
    ,((modMask , xK_p), shellPrompt myXPConfig)


    -- GridSelect
    , ((modMask, xK_g), goToSelected defaultGSConfig)

    -- layouts
    , ((modMask, xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask, xK_b ), sendMessage ToggleStruts)

    -- floating layer stuff
    , ((modMask, xK_t ), withFocused $ windows . W.sink)

    -- refresh'
    , ((modMask, xK_n ), refresh)

    -- focus
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

    -- mpd controls
    , ((0 			, 0x1008ff16 ), spawn (ssh ++ "ncmpcpp prev"))
    , ((0 			, 0x1008ff17 ), spawn (ssh ++ "ncmpcpp next"))
    , ((0 			, 0x1008ff14 ), spawn (ssh ++ "ncmpcpp play"))
    , ((0 			, 0x1008ff15 ), spawn (ssh ++"ncmpcpp pause"))

    -- scratchpad
    , ((modMask , xK_grave), scratchpadSpawnAction defaultConfig  {terminal = myTerminal})

    --Programs
    , ((modMask .|.  shiftMask, xK_u ), spawn "unison-gtk2 default")
    , ((modMask .|.  shiftMask, xK_p ), spawn "pidgin")
    , ((modMask .|.  shiftMask, xK_b ), spawn "chromium")

    -- volume control
    , ((0 			, 0x1008ff13 ), spawn "amixer -q set Master 2dB+")
    , ((0 			, 0x1008ff11 ), spawn "amixer -q set Master 2dB-")
    , ((0 			, 0x1008ff12 ), spawn "amixer -q set Master toggle")


    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q ), io (exitWith ExitSuccess))
    , ((modMask , xK_q ), restart "xmonad" True)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_r, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

