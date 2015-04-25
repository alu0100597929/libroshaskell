#!/bin/bash

sudo apt-get install python-glade2 python-appindicator python-xdg python-gconf python-pexpect
git clone https://github.com/Kilian/f.lux-indicator-applet.git
cd f.lux-indicator-applet
chmod +x setup.py
sudo ./setup.py install
fluxgui

# 28.10N, 15.39W