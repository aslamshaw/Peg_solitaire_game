program visual
    implicit none
    open (14, file='plot_graph.plt')
    write(14,*) 'set title "Inner loop counter V/S time"'
    write(14,*) 'set xrange [900:2100]'
    write(14,*) 'set yrange [8.0:9.0]'
    write(14,*) 'set xlabel "Inner loop counter"'
    write(14,*) 'set ylabel "time"'
    write(14,*) 'plot "time.txt" smooth csplines, "time.txt" with points'
    write(14,*) 'pause -1  "Hit return to continue"'
    close(14)
    
    call system('binary\wgnuplot plot_graph.plt')

    end

