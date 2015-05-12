#!/bin/bash

sudo su
mv  /var/lib/dpkg/status  /var/lib/dpkg/status-bad
cp /var/lib/dpkg/status-old  /var/lib/dpkg/status
mv /var/lib/dpkg/available  /var/lib/dpkg/available-bad
cp /var/lib/dpkg/available-old  /var/lib/dpkg/available
rm -rf /var/lib/dpkg/updates/*
rm -rf /var/lib/apt/lists/*
mkdir /var/lib/apt/lists/partial
rm /var/cache/apt/*.bin
apt-get clean
apt-get autoremove
apt-get update
dpkg --configure -a
apt-get install -f