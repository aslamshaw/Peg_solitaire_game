 set title "Inner loop counter V/S time"
 set xrange [900:2100]
 set yrange [8.0:9.0]
 set xlabel "Inner loop counter"
 set ylabel "time"
 plot "time.txt" smooth csplines, "time.txt" with points
 pause -1  "Hit return to continue"
