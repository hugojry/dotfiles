#!/bin/bash

#feh --bg-fill ~/.wallpapers/husky_jesus.jpg
#hsetroot -tile ~/.wallpapers/tile-cloudy.png
xsetroot -bitmap /home/hy/.dotfiles/wallpapers/xbm-tiles/tile.xbm -bg "#273941" -fg "#456472"

# Xresources
xrdb ~/.Xresources

# no mouse acceleration
xset m 1/1 0

# key repeat delay
xset r rate 330

# font
xset +fp /usr/share/fonts/local

# bspwm
sxhkd &
exec bspwm
