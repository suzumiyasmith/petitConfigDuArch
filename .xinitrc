#set Xresources
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

#language
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

#set env
#export PATH="${PATH}:~/.local/bin/"
#export PATH="${PATH}:~/.cabal/bin/:~/.local/bin/"
export VISUAL="vim"

#set ssh agnet
eval $(ssh-agent)

#set screen resolution
#xrandr -s 1680x1050

#set screen resolution for vmware
xrandr --newmode "1920x1080_60.00"  173.00  1920 2048 2248 2576  1080 1083 1088 1120 -hsync +vsync
xrandr --addmode Virtual1 1920x1080_60.00
xrandr --output Virtual1 --mode 1920x1080_60.00

#open vmtools
#vmtoolsd

#tranparents
xcompmgr -c &

xsetroot -cursor_name left_ptr
exec xmonad
