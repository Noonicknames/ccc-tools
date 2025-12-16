
set logscale x
set xrange [1e+3:1e+6]
set yrange [0.0:2.0]
plot \
    "./calc-rates-out/t2P<-s2S(f,n=14(full))" with l, \
    "./calc-rates-out/t2P<-s2S(g,n=16(bound))" with l, \
    "./calc-rates-out/Badnellt2P.dat" with lp