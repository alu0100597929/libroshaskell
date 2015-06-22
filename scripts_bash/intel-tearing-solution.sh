#!/bin/bash

#!!!First install mesa utilitis if u dont have it!!
sudo apt-get install mesa-utils
#To see what mesa and opengl version u have
glxinfo | grep OpenGL
sudo mkdir /etc/X11/xorg.conf.d/
echo -e 'Section "Device"\n Identifier "Intel Graphics"\n Driver "Intel"\n Option "AccelMethod" "sna"\n Option "TearFree" "true"\nEndSection' \
| sudo tee /etc/X11/xorg.conf.d/20-intel.conf
service lightdm restart