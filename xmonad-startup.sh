#!/bin/bash

## 起動するアプリケーション
# taffybar
taffybar &

# スクリーンセーバの起動
xscreensaver -no-splash &

# clipit
clipit &

# deja-dup
deja-dup-monitor &

# dropbox
dropbox start -i &

# xmodmap の設定
xmodmap $HOME/.xmodmaprc

