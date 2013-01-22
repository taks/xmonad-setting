#!/bin/bash

## 起動するアプリケーション
# taffybar
${HOME}/.cabal/bin/taffybar &

# スクリーンセーバの起動
xscreensaver -no-splash &

# clipit
clipit &

# deja-dup
deja-dup-monitor &

# dropbox
(sleep 60s && ~/.dropbox-dist/dropboxd) &

# xmodmap の設定
xmodmap $HOME/.xmodmaprc &

/usr/bin/volumeicon &

wmname LG3D &

gksu /etc/init.d/mayu restart &
