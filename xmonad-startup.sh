#!/bin/bash

## 起動するアプリケーション
# icon trayの設定
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent false --tint 0xffffff --height 20 &

# スクリーンセーバの起動
xscreensaver -no-splash &

# clipit
clipit &

# deja-dup
deja-dup-monitor &

# xmodmap の設定
xmodmap $HOME/.xmodmaprc

