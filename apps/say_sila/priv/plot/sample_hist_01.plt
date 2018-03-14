#!/usr/bin/env gnuplot
#
set title "US immigration from Northern Europe\nPlot selected data columns as histogram of clustered boxes"
set auto x
set yrange [0:300000]
set style data histogram
set style histogram cluster gap 1
set style fill solid border -1
set boxwidth 0.9
set xtic rotate by -45 scale 0
#set bmargin 10 
set term x11 persist
plot 'immigration.dat' using 6:xtic(1) ti col, '' u 12 ti col, '' u 13 ti col, '' u 14 ti col
#
