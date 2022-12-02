program visual
    implicit none

    open (14, file='plot_graph.plt')
    write(14,*) 'set title "No. of Pegs V/S time"'
    write(14,*) 'set xrange [0:32]'
    write(14,*) 'set yrange [0:5]'
    write(14,*) 'set xlabel "No. of Pegs"'
    write(14,*) 'set ylabel "time(sec)"'
    write(14,*) 'plot "time.txt" smooth csplines, "time.txt" with points'
    write(14,*) 'pause -1  "Hit return to continue"'
    close(14)
    
    call system('binary\wgnuplot plot_graph.plt')

    end

