
final_state = "t4F"

set logscale x
set xrange [1e+3:1e+6]
# set yrange [0.0:0.2]
set title sprintf("%s<-s2S Collision Strength", final_state)

plot \
    sprintf("./calc-rates-out/%s<-s2S(g,n=16(bound))", final_state) with l title "g,n=16(bound)", \
    sprintf("./calc-rates-out/%s<-s2S(f,n=14(full))", final_state) with l title "f,n=14(full)", \
    sprintf("./literature-results/Badnell/%s.dat", final_state) with l title "Badnell", \