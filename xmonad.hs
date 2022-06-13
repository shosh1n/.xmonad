{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}


import XMonad


import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Data.Monoid
import Data.Word
import Data.Bifunctor
import qualified Data.Map as M
--import XMonad.Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
-- Here starts TheMC47 Add-On
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WindowSwallowing

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
-- Here starts TheMC47 Add-On
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Hacks
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
                               hiding ( name )
--import XMonad.Layout.Magnifier
--import XMonad.Layout.ThreeColumns
-- Here starts TheMC47 Add-On
import XMonad.Layout.FixedAspectRatio
import XMonad.Layout.Gaps
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing

import XMonad.Prelude
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.OrgMode
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad

import qualified XMonad.StackSet     as W

--- Here starts TheMC47 Add-On
import XMonad.Actions.UpdatePointer
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaceGroups
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SpawnOn
import XMonad.Actions.TopicSpace
                              hiding ( TI
                                     , TopicItem
                                     , topicNames
                                     )

-- Here starts TheMC47 Add-On with Colors
myBgColor = "#2E3440"
myFgColor = "D8DEE9"

main :: IO ()
main = xmonad 
     . setEwmhActivateHook doAskUrgent
     . docks
     . dynamicSBs barSpawner
     . javaHack
     . withUrgencyHook NoUrgencyHook
     . ewmh 
    -- . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey 
     $ def
         { manageHook = myManageHook <> namedScratchpadManageHook scratchpads
         , modMask    = myModMask-- Rebind Mod to the Super key
         , workspaces = topicNames topics
         , layoutHook = myLayout -- Use custom layouts
         , handleEventHook = windowedFullscreenFixEventHook <> swallowEventHook
                               (className =? "wezterm")
                               (return True)
                                <> trayerAboveXmobarEventHook
         , logHook         = updatePointer (0.5, 0.5) (0, 0)
         , focusedBorderColor = myFgColor
         , normalBorderColor  = myBgColor
         , terminal           = myTerminal
         , startupHook         = spawn "pkill xembedsniproxy" >> addTopicGroups topics
         --, handleEventHook = setTransparentHook <> handleEventHook def --used to be with "<+>"
         }
     `additionalKeysP`([ ("M-Return", terminalHere)
                       , ("M-S-Return"
                       , namedScratchpadAction scratchpads "dropdown-term"
                         )
                       , ("M-f"   , spawn "firefox")
                       , ("M-S-d"   , rofi)
                       --, ("M-S-x" , logout)
                       , ("M-C-c" , kill)
                       , ("M-C-q", xmonadRecompile)
                        -- Prompts
                       , ("M-x"   , xmonadPrompt myXPConfig)
                       , ("M-n"
                         , workspacePrompt myXPConfig (switchTopic topicConfig)
                         )
                       , ( "M-g"
                           , promptTopicGroupView topicConfig
                                                  myXPConfig
                                                  "Go to group: "
                          )
                        , ("M-q"  , namedScratchpadAction scratchpads "discord")
                        , ("M-w"  , namedScratchpadAction scratchpads  "whatsapp")
                        , ("M-s"  , moveTo Next inUse)
                        , ("M-a"  , moveTo Prev inUse)
                        , ("M-S-s", shiftTo Next inUse)
                        , ("M-S-a", shiftTo Prev inUse)
                        , ("M-c"  , windows copyToAll)
                        , ("M-S-c", killAllOtherCopies)
                        ]
                       ++ screenKeys
                       ++ workspaceKeys
                       ++ emacsKeys
                       ++ ratioKeys
                       )
  where 
    workspaceNumbers = [1 :: Int .. 9 ] <> [0]
    workspaceKeys =
      [ ("M-" <> m <> show k, withNthWorkspace f i)  
      | (k, i) <- zip workspaceNumbers [0 ..]
      , (m, f) <- [("", W.view), ("C-", W.greedyView), ("S-", W.shift)]
      ]
    screenKeys = 
      [ ("M-" <> m <> [key], screenWorkspace sc >>= (`whenJust` windows . f))
      | (key, sc) <- zip "po" [0 ..]
      , (f  , m ) <- [(W.view, ""), (W.shift, "S-")]
      ]
      ++ [ ( "M-C-" <> [key] 
           , screenWorkspace sc >>= (`whenJust` windows . W.greedyView)
           )
         | (key, sc) <- zip "op" [0 ..]  
         ]
    emacsKeys = makeSubmap
      "e"
      (spawn emacs)
      [ ("t"  , namedScratchpadAction scratchpads "todos")
      , ("o"  , orgPrompt myXPConfig "TODO" "org/todos.org")
      , ("S-o", orgPromptPrimary myXPConfig "TODO" "org/todos.org")
      , ("r"  , spawn "systemctl restart --user emacs")
      ]
    mkRatioAction = (>> refresh) . withFocused . (broadcastMessage .)
    ratioKeys     = makeSubmap "a" (mkRatioAction $ ToggleRatio (16 / 9)) $ map
      (second mkRatioAction)
      [ ("r", ResetRatio)
      , ("6", FixRatio (16 / 9))
      , ("4", FixRatio (4 / 3))
      , ("c", FixRatio (6 / 7))
      ]
    inUse = hiddenWS :&: Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag]
-- | Helper function to make submaps. For the given key k,
-- bind @M-k@ to the given action, and prefixes the keys in the
-- list wiht @M-C-k@
makeSubmap :: String -> X () -> [(String, X ())] -> [(String, X ())]
makeSubmap k action ks = 
  ("M-" <> k, action) : map (first (("M-S-" <> k <> " ") <>)) ks

myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces = zipWith
  (<>)
  (map ((<> ":") . show) [(1 :: Int) .. 10])
  [ book
  ,
      --
    "\xf120"
  ,
      --
    "\xf268"
  ,
      --
    "\xf108"
  ,
      --
    "\xf108"
  , 
      --
    "\xf108"
  ,
      --
    "\xf108"
  ,
      --
    "\xf095"
  ,
      --
    "\xf2dc"
  ,  
      --
    "\xf11b \xf236"
    ]

book :: String
book = "\xf02d"

{--
 === Topics ===
 A small DSL for topics. Allegedly inspired by byorgey's TopicItem, and was
 expanded to to work with DynamicWorkspacesGroups

 --}

 -- | Convenience for standard topics.
data TopicItem
  = TI
      { -- | 'Topic' = 'String'
       name :: Topic,
       -- | Directory associated with topic; 'Dir' = 'String'
       dir :: Dir,
       -- | Startup hook when topic is empty
       action :: X()
      }
  | TG
      { name :: Topic,
        dir :: Dir,
        actions :: [(ScreenId, X ())]
      }

myDirs :: [TopicItem] -> M.Map Topic Dir
myDirs = M.fromList . concatMap go 
  where
    go TI {..} = [(name, dir)]
    go TG {..} = map (bimap (`getTopicName` name) (const dir)) actions

myActions :: [TopicItem] -> M.Map Topic (X ())
myActions = M.fromList . concatMap go
  where 
    go TI {..} = [(name, action)]
    go TG {..} = map (first (`getTopicName` name)) actions

topicNames :: [TopicItem] -> [String]
topicNames = concatMap go
  where
    go TI {..} = [name]
    go TG {..} = map ((`getTopicName` name) . fst) actions

getTopicName :: ScreenId -> Topic -> Topic
getTopicName 0     t = t
getTopicName (S n) t = t <> " - " <> show (n + 1)

genTopicConfig :: [TopicItem] -> TopicConfig
genTopicConfig ts = def { topicDirs          = myDirs ts
                        , topicActions       = myActions ts
                        , defaultTopicAction = const mempty
                        , defaultTopic       = name $ head ts
                        }

topics ::  [TopicItem]
topics = 
  map only myWorkspaces
    ++ [ TG 
         "Master"
         "Studium/thesis"
         [(0, spawnHere emacs), (1, browse "https://shosh1n.github")]
       ]
  where
    noAction :: Topic -> Dir -> TopicItem
    noAction n d = TI n d mempty
    only :: Topic -> TopicItem
    only n = noAction n "./"

formalMethodsGroup :: String
formalMethodsGroup = book <> " Formal Methods"

kivworkspace :: String
kivworkspace = formalMethodsGroup <> " - 2"

addTopicGroup :: TopicItem -> X ()
addTopicGroup TI{}   =  mempty
addTopicGroup TG{..} = addRawWSGroup name . reverse $ map
  (\(sid, _) -> (sid, getTopicName sid name))
  actions

addTopicGroups :: [TopicItem] -> X()
addTopicGroups = mapM_ addTopicGroup

topicConfig :: TopicConfig
topicConfig = genTopicConfig topics

--------
--- Scratchpads
--------


scratchpads = 
  [ NS "todos"
       todoCommand
       (title =? todoTitle)
       (customFloating $ W.RationalRect (1 / 6) 0 (2 / 3) (1 / 3))
  , NS "dropdown-term"
       (inTerminal .withTitleT $ dropDownTitle)
       (title =? dropDownTitle)
       (customFloating $ W.RationalRect (1 / 6) 0 (2 / 3) (2 / 3))
  , NS "discord"
       "discord"
       (className =? "Discord")
       (customFloating $ W.RationalRect (1 / 8) (3 / 4) (3 / 4) (1 / 4))
  , NS "signal"
       "signal-dekstop"
       (className =? "Signal")
       (customFloating $ W.RationalRect (2 / 3) (1 / 8) (1 / 3) (3 / 4))
  ]
  where
    todoTitle      = "TODOs"
    todoCommand    = inEmacs . eWithTitle todoTitle $ "org/todos.org"
    dropDownTitle  = "Dropdown"
--------
--- Emacs
--------

myTerminal :: String
myTerminal = "wezterm "

inTerminal = (myTerminal <>)

withTitleT :: ShowS
withTitleT c = "--title " <> c <> " "

inDirT :: ShowS
inDirT dir = "--working-directory \"" <> dir <> "\""

terminalHere :: X ()
terminalHere = spawnHere . inTerminal . inDirT =<< currentTopicDir topicConfig

emacs :: String
emacs =  "emacsclient -a '' -create-frame --no-wait "

eWithTitle :: String -> ShowS
eWithTitle t = (("-F '(quote (name . \"" <> t <> "\"))' ") <>)

inEmacs :: ShowS
inEmacs = (emacs <>)

openP :: ShowS
openP dir = "--eval '(open-magit-or-dired " <> dir <> ")'"

openDir :: X ()
openDir = spawnHere . inEmacs .openP . quote =<< currentTopicDir topicConfig

magit :: ShowS
magit dir = "--eval '(magit-status \"" <> dir <> "\")'"

magitHere :: X ()
magitHere = spawnHere . inEmacs . magit =<< currentTopicDir topicConfig

fileInDir :: String -> ShowS
fileInDir dir = ((dir <> "/") <>)

openFileInDir :: String -> X ()
openFileInDir  file =
  spawnHere
    .  inEmacs
    .  quote
    .  (`fileInDir` file)
    =<< currentTopicDir topicConfig

browse :: String -> X ()
browse = spawnHere . ("firefox --new-window " <>)

quote :: ShowS
quote s = "\"" <> s <> "\""

---------
--- Actions
--------

logout :: X()
logout = 
  spawn 
    "dbus-send --print-reply --dest=xorg.x11.xserver /Xserver xorg.x11.XserverInterface.logout int32:1 int32:0 int32:1"

rofi :: X()
rofi = spawn "rofi -m -4 -modi run,drun -show drun"

xmonadRecompile :: X ()
xmonadRecompile = spawn "xmonad --recompile; xmonad --restart"

toggleFullScreen :: X ()
toggleFullScreen = do
  sendMessage $ Toggle NBFULL
  toggleCollapse

toggleCollapse :: X ()
toggleCollapse = do
  sendMessage ToggleStruts
  toggleWindowSpacingEnabled
  sendMessage ToggleGaps

workspaceAt :: Int -> String
workspaceAt 0 = last myWorkspaces
workspaceAT n = ((myWorkspaces !!) . pred) n

---------------------
--- Layouts - finally!
---------------------

myLayout = 
  renamed [KeepWordsRight 1]
    .  avoidStruts
    .  fixedAspectRatio (0.5, 0.5)
    .  layoutHintsWithPlacement (0.5, 0.5)
    .  spacingRaw False (Border 0 0 0 0) True (Border 10 10 10 10 ) True -- between windows
    .  gaps [(U, 15), (R, 15), (L, 15), (D, 15)]
    .  mkToggle (single NBFULL) -- toggle full screen
    .  smartBorders
    .  onWorkspace kivworkspace simplestFloat
    $  tiled 
    ||| mtiled 
    ||| full
  where 
    tiled  = renamed [Replace "T "] $ ResizableTall 1 (3 / 100) (5 / 8) []
    mtiled = renamed [Replace "MT"] $ Mirror tiled
    full   = renamed [Replace "F "] Full

------------------
--- ManageHook
------------------

willFloat :: Query Bool
willFloat = ask >>= \w -> liftX $ withDisplay $ \d -> do
  sh <- io $ getWMNormalHints d w
  let isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
  isTransient <- isJust <$> io (getTransientForHint d w)
  return (isFixedSize || isTransient)

myManageHook :: ManageHook
myManageHook =
  manageSpawn
    <> (not <$> willFloat --> insertPosition Below Newer)
    <> mconcat manageHooks
    <> (isDialog --> doFloat)
  where
    manageHooks = generalRules ++ concat windowRules
    generalRules =
      [ className =? "discord" --> doShift (workspaceAt 8)
      , className
        =?  "sun-awt-X11-XFramePeer"
        <||> className
        =? "Main"
        --> doShift kivworkspace
      , title =? "Netflix" <||> className =? "vlc" --> doFixAspect (16 / 9)
      ] 
    windowRules = 
      [ [ className =? c --> doFloat | c <- floatsClasses ]
      , [ title =? t --> doFloat | t <- floatsTitles ]
      ]
    floatsClasses =
      [ "MPlayer"
      , "Gimp"
      , "yakauke"
      , "lightdm"
      , "R_x11"
      ]
    floatsTitles = ["alsamixer"]

---------------
--- Status Bar -- finally!
----------------


circleSep::String
circleSep =
 "<icon=circle_right.xpm/></fc> <fc=#D8DEE9,#2E3440:0><icon=circle_left.xpm/>"

topPP :: X PP
topPP =
  copiesPP (xmobarColor "green" "")
  <=< clickablePP
  .  filterOutWsPP [scratchpadWorkspaceTag]
  $  def { ppCurrent = xmobarBorder "Bottom" myFgColor 4
         , ppUrgent  = xmobarBorder "Bottom" "#CD3C66" 4
         , ppVisible = xmobarBorder "Bottom" "#98a0b3" 1
         , ppSep     = circleSep
         , ppExtras  = [logLayoutOnScreen 0, shortenL 50 (logTitleOnScreen 0)]
         , ppOrder   = \(ws : _ : _ : extras) -> ws : extras
         }

secondaryPP :: ScreenId -> X PP
secondaryPP s = pure $ def
  { ppOrder   = \(_ : _ : _ : extras) -> extras 
  , ppSep     = circleSep
  , ppExtras  = [ logCurrentOnScreen s
                , logLayoutOnScreen s
                , shortenL 50 $ logTitleOnScreen s
                , logWhenActive s (logConst "*")
                ]
  }

barSpawner :: ScreenId -> IO StatusBarConfig
barSpawner 0       = pure $ statusBarProp "xmobar top" topPP <> trayerSB
barSpawner s@(S n) = pure $ statusBarPropTo
  ("_XMONAD_LOG__Secondary_" <> show n)
  ("xmobar secondary " <> show n)  
  (secondaryPP s)

trayerSB :: StatusBarConfig
trayerSB = statusBarGeneric
  (unwords
    [ "trayer"
    , "--edge top"
    , "--align right"
    , "--widthtype request"
    , "--expand true"
    , "--monitor primary"
    , "--transparent true"
    , "--alpha 0"
    , "-l"
    , "--tint 0x2E3440"
    , "--height 30"
    , "--margin 27"
    ]
  )
  mempty

------------------
--- Prompt
-----------------

myXPConfig :: XPConfig
myXPConfig = def { font = "xft:Source Code Pro:size14:regular:antialias=true"
                 , position        = CenteredAt (1 / 4 ) (2 / 3)
                 , bgColor         = myBgColor
                 , fgColor         = myFgColor
                 , bgHLight        = myFgColor
                 , fgHLight        = myBgColor
                 , borderColor     = myFgColor
                 , searchPredicate = fuzzyMatch
                 , sorter          = fuzzySort
                 , height          = 30
                 }












myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = magenta " . "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow
   
    -- | Windows should have *some* title, which shouldn't exceed a sane length.

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String 
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

setTransparentHook :: Event -> X All
setTransparentHook ConfigureEvent{ ev_event_type = createNotify, ev_window = id} = do
  setOpacity id opacity
  return (All True) where
  opacityFloat = 0.9
  opacity = floor $ fromIntegral (maxBound :: Word32) * opacityFloat
  setOpacity id op = spawn $ "xprop -id " ++ show id ++ " -f _NET_WM_WINDOW_OPACITY 32c _NET_WM_WINDOW_OPACITY " ++ show op
setTransparentHook _ = return (All True)

