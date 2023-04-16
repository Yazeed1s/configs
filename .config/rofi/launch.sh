#!/usr/bin/sh

dir="$HOME/.config/rofi/"
theme='bar'

## Run
rofi \
    -show run \
    -theme ${dir}/${theme}.rasi
