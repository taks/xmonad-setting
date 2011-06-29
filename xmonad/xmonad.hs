
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
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

-- terminal
terminalCommand = "lxterminal"
terminalClass = "Lxterminal"

-- ウィンドウ作成時のデフォルトワークスペース
myManageHookShift = composeAll
    [ className =? terminalClass    --> viewShift "1"
    , className =? "Iceweasel"      --> viewShift "2"
    , className =? "Emacs"          --> viewShift "3"
    , className =? "com-mathworks-util-PostVMInit" --> viewShift "4"
    ]
    where viewShift = doF . liftM2 (.) W.view W.shift

-- キーバインド
myKeys :: [(String, X ())]
myKeys =
  [("M-S-l", spawn "xscreensaver-command -lock")
  , ("M-e e", runOrRaise "emacs" (className =? "Emacs"))
  , ("M-e b", runOrRaise "iceweasel" (className =? "Iceweasel"))
  , ("M-e t", runOrRaise terminalCommand (className =? terminalClass))
  , ("<F12>", runOrRaise terminalCommand (className =? terminalClass))
  , ("M-S-<Return>", nextScreen)
  , ("<F11>", sendMessage $ Toggle FULL)]

tall = Tall 1 (3/100) (1/2)
myLayout =  avoidStruts $ mkToggle1 FULL $ desktopLayoutModifiers (named "V" tall ||| (named "H" $ Mirror tall))

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
            {
              modMask = mod3Mask
            , startupHook = setWMName "LG3D" -- for matlab
            , manageHook = myManageHookShift
                           <+> manageDocks
                           <+> manageHook defaultConfig
            , layoutHook = myLayout
            , logHook = dynamicLogWithPP $ xmobarPP
                            { ppOutput = hPutStrLn xmproc
                            , ppTitle = xmobarColor "green" "" . shorten 100
                            }
            , terminal           = "gnome-terminal"
            , borderWidth        = 4
            , normalBorderColor  = "#333333"
            , focusedBorderColor = "#cd8b00"
            } `additionalKeysP` myKeys
