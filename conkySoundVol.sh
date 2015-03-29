#!/bin/sh
amixer sget Master | awk -F"[][]" '/dB/ { if($6=="on") print $2; else print "Muted"; }'
