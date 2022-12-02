 set title "No. of Pegs V/S time"
 set xrange [0:32]
 set yrange [0:5]
 set xlabel "No. of Pegs"
 set ylabel "time(sec)"
 plot "time.txt" smooth csplines, "time.txt" with points
 pause -1  "Hit return to continue"
