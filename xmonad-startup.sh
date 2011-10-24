#!/bin/bash

## 起動するアプリケーション
# taffybar
~/.cabal/bin/taffybar &

# スクリーンセーバの起動
xscreensaver -no-splash &

# clipit
clipit &

# deja-dup
deja-dup-monitor &

# xmodmap の設定
xmodmap $HOME/.xmodmaprc

