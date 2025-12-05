plot \
    "./calc-rates-out/auto_gauss" using 1:2 title "auto gauss" with lp, \
    "./calc-rates-out/linear" using 1:2 title "linear" with lp, \
    "./calc-rates-out/monotone_cubic_spline" using 1:2 title "monotone cubic" with lp, \
    "./calc-rates-out/natural_cubic_spline" using 1:2 title "natural cubic" with lp, \
