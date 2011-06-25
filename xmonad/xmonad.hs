
import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.SetWMName

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
            {
              modMask = mod3Mask
            , startupHook = setWMName "LG3D"
            , manageHook = manageDocks <+> manageHook defaultConfig
            , layoutHook = avoidStruts $ layoutHook defaultConfig
            , logHook = dynamicLogWithPP $ xmobarPP
                            { ppOutput = hPutStrLn xmproc
                            , ppTitle = xmobarColor "green" "" . shorten 50
                            }
            , terminal           = "gnome-terminal"
            , borderWidth        = 2
            , normalBorderColor  = "#333333"
            , focusedBorderColor = "#cd8b00"
            }
