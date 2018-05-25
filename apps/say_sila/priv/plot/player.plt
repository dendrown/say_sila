#!/usr/bin/env gnuplot
#
unset logscale
unset label

set title "Big Players by Tweets (TT)"
set xlabel "Players (300 of 67658)"
set ylabel "Tweets"
set xtic auto
set ytic auto
set xrange [0:300]
set yrange [0:1500]
set style data histogram
#et style fill solid border -1
set boxwidth 1.4
set arrow from 13, graph 0 to 13, graph 1 nohead lc 3 lt 5
set arrow from 43, graph 0 to 43, graph 1 nohead lc 3 lt 5

set term x11 persist
plot 'player.gw.dat' using 1 title "GW Tweets"
#
