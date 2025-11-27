plot \
    "./calc-rates-out/auto_integration_grid" using 1:2 title "auto" with lp, \
    "./calc-rates-out/linear_grid" using 1:2 title "linear" with lp, \
    "./calc-rates-out/quadratic_grid" using 1:2 title "quadratic" with lp, \
    "./calc-rates-out/cubic_grid" using 1:2 title "cubic" with lp, \
    "./calc-rates-out/weird_grid" using 1:2 title "weird" with lp, \
