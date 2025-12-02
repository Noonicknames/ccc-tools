

#CFLAGS := -O3 -xHost -ip -fpp -qopenmp -no-wrap-margin -fp-model precise -prec-div \
            -prec-sqrt -qoverride-limits -traceback -reentrancy threaded \
	    -mcmodel=medium -shared-intel -mkl -r8 -align dcommons -heap-arrays \
	    -h PIC -diag-disable 8291 -parallel 

#CFLAGS := -O3 -qopenmp -parallel -xHost -g

#FC := ifort
#FC := ftn
FC := gfortran
CFLAGS := -ffree-line-length-512 -O3 -fopenmp 

#FLAGS :=   -Wl,--start-group \
            $(MKLROOT)/lib/intel64/libmkl_intel_lp64.a \
    	    $(MKLROOT)/lib/intel64/libmkl_intel_thread.a \
  	    $(MKLROOT)/lib/intel64/libmkl_core.a -Wl,--end-group -l pthread -lm

#CFLAGS := $(CFLAGS) -g -O0 -traceback -check all 

main : main.f90
	$(FC) $(CFLAGS) $(LFLAGS) main.f90 -o main

clean :
	rm main *.mod
