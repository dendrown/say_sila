#!/usr/bin/env gnuplot
#
unset logscale
unset label

set title "Big Players by Tweets (TT)"
set xlabel "Players"
set ylabel "Tweets"
set xtic auto
set ytic auto
set xrange [0:500]
set yrange [0:1500]
set style data histogram
set style fill solid border -1
set boxwidth 0.9

set term x11 persist
plot 'player.gw.dat' using 1 title "GW Tweets"
#
