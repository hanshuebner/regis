#!/bin/sh

cd emulate
mame vt240 -window -host null_modem -bitb socket.127.0.0.1:11313 -r 800x480 -skip_gameinfo -inipath .
