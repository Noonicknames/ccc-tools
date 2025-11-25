plot \
    for [i=1:2] sprintf("./calc-rates-out/grid%d", i) using 1:2 title sprintf("grid%d", i) with lp, \
    "./calc-rates-out/gridauto" using 1:2 title "gridauto" with lp,
