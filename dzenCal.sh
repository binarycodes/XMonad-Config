#!/bin/bash

FONT="-*-droid sans mono-medium-r-normal-*-12-*-*-*-*-*-*-*"
CRIT="#E62F58"

TODAY=$(expr $(date +'%d') + 0)
MONTH=$(date +'%m')
YEAR=$(date +'%Y')

(
    echo '^bg(#101010)^fg(#111111)'
    echo '          ^fg(#9ABD02)CALENDAR'

# current month, highlight header and today
    cal -m | sed -r -e "1,2 s/.*/^fg(#D09D3B)&^fg()/" -e "s/(^| )($TODAY)($| )/\1^bg()^fg($CRIT)\2^fg()^bg()\3/" -e "s/^/    /"

# next month, hilight header
    [ $MONTH -eq 12 ] && YEAR=$(expr $YEAR + 1) && MONTH=0
    cal -m $(expr $MONTH + 1) $YEAR | sed -e "1,2 s/.*/^fg(#D09D3B)&^fg()/" -e "s/^/    /"
) \
    | dzen2 -p 10 -fn "$FONT" -x 1730 -y 14 -w 190 -l 18 -sa l -e 'onstart=uncollapse;button3=exit'
