
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWS
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W
-- キーバインド追加のため
import XMonad.Util.EZConfig
-- レイアウト変更のため
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Layout.Named
import DBus.Client.Simple
import System.Taffybar.XMonadLog ( dbusLogWithPP, taffybarEscape, taffybarColor )
-- for wmctrl
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Util.SpawnOnce

-- terminal
terminalCommand = "terminator"
terminalClass   = "Terminator"

-- ウィンドウ作成時のデフォルトワークスペース
-- className は，xprop | grep WM_CLASS で調べられる
myManageHookShift = composeAll
    [ className =? terminalClass    --> viewShift "1"
    , className =? "Firefox"        --> viewShift "2"
    , className =? "Emacs"          --> viewShift "3"
    , className =? "com-mathworks-util-PostVMInit" --> viewShift "4"
    , className =? "Eclipse" --> viewShift "5"
    , className =? "Titanium Studio" --> viewShift "5"
    , className =? "SDL_App" --> viewShift "5"
    , className =? "Okular" --> viewShift "6"
    ]
    where viewShift = doF . liftM2 (.) W.view W.shift

-- キーバインド
myKeys :: [(String, X ())]
myKeys =
  [("M-S-l", spawn "xscreensaver-command -lock")
  , ("M-e e", runOrRaise "emacs" (className =? "Emacs"))
  , ("M-e b", runOrRaise "firefox" (className =? "Firefox"))
  , ("M-e t", runOrRaise terminalCommand (className =? terminalClass))
  , ("<F12>", runOrRaise terminalCommand (className =? terminalClass))
  , ("M-S-<Return>", nextScreen)
  , ("M-w", nextScreen)
  , ("M-r" , swapNextScreen)
  , ("<F11>", sendMessage $ Toggle FULL)]

tall = Tall 1 (3/100) (1/2)
myLayout =  avoidStruts $ mkToggle1 FULL $ desktopLayoutModifiers (named "V" tall ||| (named "H" $ Mirror tall))

-- taffybar
taffybarPP :: PP
taffybarPP = defaultPP { ppCurrent         = taffybarColor "yellow" "" . taffybarEscape . wrap "[" "]"
                       , ppVisible         = taffybarEscape . wrap "(" ")"
                       , ppHidden          = taffybarEscape
                       , ppHiddenNoWindows = taffybarEscape
                       , ppUrgent          = taffybarColor "red" "yellow" . taffybarEscape
                       , ppTitle           = taffybarColor "green" "". taffybarEscape
                       , ppLayout          = taffybarEscape
                       }

wl3d :: XConfig a -> XConfig a
wl3d c = c { startupHook = startupHook c >> setWMName "LG3D" }

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "taffybar"
  spawnOnce "xscreensaver -no-splash"
  spawnOnce "deja-dup-monitor"
  spawnOnce "(sleep 60s && ~/.dropbox-dist/dropboxd)"
  spawnOnce "volumeicon"
  spawnOnce "clipit"
  spawnOnce "xmodmap ~/.xmodmaprc"
  spawnOnce "~/.gem/ruby/1.9.1/bin/xkeyremap"

-- main
main = do
  client <- connectSession
  let pp = taffybarPP
  xmonad $ wl3d $ ewmh defaultConfig
            {
              modMask = mod3Mask
            , manageHook = myManageHookShift
                           <+> manageDocks
                           <+> manageHook defaultConfig
            , startupHook = myStartupHook
            , layoutHook = myLayout
            , logHook = dbusLogWithPP client pp
            , terminal           = terminalCommand
            , borderWidth        = 4
            , normalBorderColor  = "#333333"
            , focusedBorderColor = "#cd8b00"
            } `additionalKeysP` myKeys
