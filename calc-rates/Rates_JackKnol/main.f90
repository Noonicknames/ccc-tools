module functions

contains

function mcsplines(X_in,Y_in,X_out)
  implicit none
  real*8, intent(in) :: X_in(0:), Y_in(0:), X_out(0:)
  real*8, allocatable :: mcsplines(:)
  integer :: num_X_in, num_X_out, k, j, low, mid, high
  real*8, allocatable :: Delta(:), m(:)
  real*8 :: alpha, beta, h, t
  real*8 :: h00, h10, h01, h11

  num_X_in = size(X_in)
  if(size(Y_in).ne.num_X_in) then
    print*, num_X_in, size(Y_in)
    stop ' STOPPING in mcsplines: num_X_in =/= num_Y_in'
  endif
  num_X_out = size(X_out)
  allocate(mcsplines(0:num_X_out-1))

  allocate(Delta(0:num_X_in-2))
  allocate(m(0:num_X_in-1))
  do k=0, num_X_in-2
    Delta(k) = (Y_in(k+1)-Y_in(k))/(X_in(k+1)-X_in(k))
    if(k.gt.0) then
      m(k) = (Delta(k-1)+Delta(k))/2.0d0
      if(sign(1.0d0,Delta(k)).ne.sign(1.0d0,Delta(k-1))) m(k)=0.0d0
    endif
  enddo
  m(0) = Delta(0)
  m(num_X_in-1)=Delta(num_X_in-2)
  do k=0, num_X_in-2
    if(Delta(k).eq.0.0d0) then
      m(k) = 0.0d0
      m(k+1) = 0.0d0
    else
      alpha=m(k)/Delta(k)
      if(k.gt.0) then
        beta=m(k)/Delta(k-1)
      else
        beta=0.0d0
      endif
      if(alpha.lt.0.d0.or.beta.lt.0.0d0) m(k) = 0.0d0
      if(alpha**2+beta**2>9.0d0) then
        m(k) = 3.0d0*alpha*Delta(k)/sqrt(alpha**2+beta**2)
        m(k+1) = 3.0d0*beta*Delta(k)/sqrt(alpha**2+beta**2)
      endif
    endif
  enddo
  !definitions of the hermite basis functions taken from
  !https://en.wikipedia.org/wiki/Cubic_Hermite_spline
  do k=0,num_X_out-1
    if(X_out(k).le.X_in(0)) then
      mcsplines(k) = Y_in(0)
    elseif(X_out(k).ge.X_in(num_X_in-1)) then
      mcsplines(k) = Y_in(num_X_in-1)
    else
      low=minloc(abs(X_in(:)-X_out(k)),1)-1
      if(X_in(low).lt.X_out(k)) then
        high=low+1
      elseif(X_in(low).gt.X_out(k)) then
        high=low
        low=high-1
      else
        mcsplines(k) = Y_in(low)
        cycle
      endif

      h = X_in(high)-X_in(low)
      t = (X_out(k) - X_in(low))/h
      h00 = 2.0d0*t**3 - 3.0d0*t**2 + 1.0d0
      h01 = -2.0d0*t**3 + 3.0d0*t**2
      h10 = t**3 - 2.0d0*t**2 + t
      h11 = t**3 - t**2
      mcsplines(k) = Y_in(low)*h00 + h*m(low)*h10 + Y_in(high)*h01 + h*m(high)*h11
    endif
  enddo
end function mcsplines

end module functions

module subroutines

contains

subroutine initialiseFile(filename, nunit, num_data)

    !Preprocesses a file with # comments at the start of the file to start reading at the data
        implicit none
        character(len=:), allocatable, intent(in) :: filename
        integer, intent(out) :: nunit
        integer, intent(out) :: num_data
        integer :: num_comm, ii, io
        character :: tempchar
        logical :: file_exists

    inquire(file=filename,exist=file_exists)
    if(file_exists) then
        open(newunit=nunit, file=filename, action='read')
        else
            print*, 'Error: cannot find file '//filename
        stop
    endif

        num_comm = 0
        num_data = 0

        do
            read(nunit,*,iostat=io) tempchar
            if (io/=0) then
                exit
            else
                if (tempchar == '#') then
                    num_comm = num_comm + 1
                else
                    num_data = num_data + 1
                endif
            endif
        end do
        rewind (nunit)
        do ii = 1, num_comm
            read(nunit, *)
        enddo
end subroutine initialiseFile

SUBROUTINE CalcRates(vi_f,tg,T,conv)
use functions
implicit none

integer  vi_f, i, n, j, tg, m, io, num_lines, conv, nunit, vi_max
Real*8, allocatable :: vibenergy(:), E_in(:), sigma_in(:), E(:), sigma(:), w(:)
Real*8, allocatable :: func(:)
Real*8 :: T(tg), rates(tg), gamma_v(0:vi_f), z(tg), rates_sum(tg), rates_lte(tg)
Real*8 :: integral, k, pi, m_e, Emax, dx, h, junk
Character(len=200) :: filename, filename4, filename5, tempchar
Character(len=:), allocatable :: datadir, fileformat, targetmol, filename2, filename3
integer, external :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
real*8 :: temp
logical :: file_exists


!------------------------------------------------------------------------------------
!                                      ON INPUT
!                vi_f          is the final value of the initial vibrational level loop
!                tg            is the size of the temperature grid
!                T(tg)         is a one dimensional array representing the temperature grid
!                conv          is an integer value that allows for the final rates to be converted to cm^3/s (specified in the input folder)
!------------------------------------------------------------------------------------

k = 3.166811429*(10.0**(-6.0))                             !boltzman constant   (E_h/K)m_e = 1.0                                                  
m_e = 1.0                                                  !Electron mass       (atomic units)
pi = 3.1415926535897932
dx = 0.02d0                                                !Determines energy grid points (n = Emax/dx) and used to create weighting in numerical integration
z = 0.0d0                                                  !Partition function
rates_sum = 0.0d0                                          !Sum over vibrational levels used for LTE

!read in target molecule type (H2, D2, T2, HT, DT, HT)
print*, 'Enter target molecule'
read(*,*) tempchar
targetmol = trim(adjustl(tempchar))

!read in specific directory and transition
print*, 'Enter root data directory'
read(*,'(A)') tempchar
datadir = trim(adjustl(tempchar))

print*, 'Enter transition/dissociation filename format'
read(*,*) tempchar
fileformat = trim(adjustl(tempchar))

print*, 'Enter max initial vibrational level'
read(*,*) vi_max

!checks input file for a specified max vi_f, otherwise the script will take the max level from the directory
if (vi_f==99) then
    vi_f = vi_max
endif


!assign filename for vibrational energy file (checks for .txt or not)
WRITE (tempchar, '(a,"/vibrational_energies/vibenergies-",a,"-X1Sg.txt")') datadir, targetmol
filename3 = trim(adjustl(tempchar))
inquire(file=filename3,exist=file_exists)
if (.not.file_exists) then
    WRITE (tempchar, '(a,"/vibrational_energies/vibenergies-",a,"-X1Sg")') datadir, targetmol
    filename3 = trim(adjustl(tempchar))
end if

!Create directory structure for rate coefficients 
do i=0,vi_f
     write(tempchar,'(a,"/Rate_coefficients/vi=",I0)') datadir, i
     call system('mkdir -p ' //trim(adjustl(tempchar)))
enddo
write(tempchar,'(a,"/Rate_coefficients/Lte")') datadir
call system('mkdir -p ' //trim(adjustl(tempchar)))


!Ignore comment lines, allocate array for vib energy and read in values (in hartrees)
call initialiseFile(filename3,nunit,num_lines)
allocate(vibenergy(0:num_lines-1))
    do i=0, num_lines-1
        read(nunit,*) junk ,vibenergy(i)
    end do
close(nunit)



!Vibrational energy used in rates formula is the difference from ground level energy
do i=0, vi_f
    gamma_v(i) = vibenergy(i) - vibenergy(0)
end do

print*, 'Working in directory:', datadir
print*, 'Calculating rates for ', fileformat, ' with vi_f=', vi_f
print*, 'NUM THREADS:', omp_get_max_threads()


!vibration level loop begins
!---------------------------------------------------------------------------------------------
!!$OMP PARALLEL DO & 
!!$OMP DEFAULT(PRIVATE) &
!!$OMP SHARED(pi,m_e,k,datadir,fileformat,gamma_v,T,dx,tg,vi_f,conv) &
!!$OMP REDUCTION(+:rates_sum) REDUCTION(+:z) &
!!$OMP SCHEDULE(GUIDED) 
do m=0, vi_f

    !read in energy and cross section (sigma_m) data from file and assign
    WRITE (tempchar, '(a,"/Cross_sections/vi=",I0,"/",a,"_vi=",I0,".txt")') datadir, m, fileformat, m
    filename2 = trim(adjustl(tempchar))
    call initialiseFile(filename2,nunit,num_lines)
    allocate(E_in(num_lines), sigma_in(num_lines))
    E_in = 0.0d0
    sigma_in = 0.0d0
       do i=1, num_lines
            read(nunit,*) E_in(i), sigma_in(i)
       end do
    close(nunit)
    
    !set max energy value to Emax for energy grid
    Emax = E_in(num_lines)


    !number of subdivisions for simpsons rule (numerical integration)
    n = Emax/dx                       


    !Set n even
    if (mod(n,2) /= 0) then
        n = n + 1
    end if
  
    
    !allocation of arrays 
    Allocate(E(0:n), w(0:n))
    w = 0.0d0
    E = 0.0d0
    

    !Create array containing weights for simpsons rule (dx/3.0)*[1,2,4,2,4,......,1]
    do i=1, n
        w(i) = 2.0 + mod(i,2)*2.0
    end do
    w(0)=1.0
    w(n)=1.0
    w = ((dx/3.0d0)*w)/27.211386245988            !coverting dx unit of energy to hartree
    

    !Energy Grid
    do i=0, n
      E(i)= i*dx
    end do
    !convert energy grid in eV to Hartree
    E = E/27.211386245988
    
 
    Allocate (func(0:n), sigma(0:n))
 

    !convert E_in values from eV to Hartree energy
    E_in = E_in/27.211386245988
 
     
    !use cubic spline function to interpolate input cross sections onto energy grid E
    sigma = mcsplines(E_in,sigma_in,E)
 

    !Sigma below threshold energy equal to zero
    do i=0, n
        if (E(i) < E_in(1)) then
            sigma(0:i) = 0.0d0
        end if
    end do


    !rates for current sigma_v
    !------------------------------------------------------------------------
    !Numerical integration
    !multiply cross section by coefficients that do not involve T
    sigma = sigma * w * E

    !Loop to calculate integral for current T, followed by the rate as a function of T
    do j=1, tg                                                                     
    func = 0.0d0
        temp = k*T(j)
        integral =  sum( sigma(:)*exp(-(E(:)/(temp))) )                             
        rates(j) = (8.0*pi/sqrt(m_e))*((1.0/(2.0*pi*k*T(j)))**(3.0/2.0))*integral
    end do
    !-----------------------------------------------------------------------


 
    !add to sum of rates for LTE
    do j=1, tg
        rates_sum(j) = rates_sum(j) + (rates(j)*exp(-gamma_v(m)/(k*T(j))))
    end do
    
    !array containting value of the partition function z(T)
    do j=1, tg
        z(j) = z(j) + exp(-gamma_v(m)/(k*T(j)))
    end do
    
    
    !convert rates to cm^3/s
    if (conv == 1) then
        rates = rates*(6.126159E-9)
    end if

    
    !write rates for current v_i
    WRITE (tempchar, '(a,"/Rate_coefficients/vi=",I0,"/",a,"_vi=",I0,"_RC.txt")') datadir, m, fileformat, m
    filename = trim(adjustl(tempchar))
        OPEN (110+m, FILE=trim(adjustl(filename)), action='write', status='replace')
        if (conv==1) then
            write(110+m,*) "# Electron.Temp(eV)       Rate.Coefficient(cm^3/s)"
        else
            write(110+m,*) "# Electron.Temp(eV)       Rate.Coefficient(a.u.)"
        endif
    
        do j=1, tg
            write(110+m,*) 27.211386245988 * k * T(j),  rates(j)
        end do
    close(110+m)
    
    deallocate(E_in, Sigma_in, E, w, func, sigma)


end do
!------------------------------------------------------------------------------------------------
!vibrational level loop ends
!!$OMP END PARALLEL DO


!LTE rate calculated from sums
do j=1, tg
    rates_lte(j) = rates_sum(j)/z(j)
end do


!convert LTE to cm^3/s
if (conv==1) then
    rates_lte = rates_lte*(6.126159E-9)
end if


!Write thermallly averaged rate to file!
WRITE (tempchar, '(a,"/Rate_coefficients/Lte/",a,"_RC-Lte.txt")') datadir, fileformat
filename2 = trim(adjustl(tempchar))
open(98,file=(filename2),action='write',status='replace')
    do i=1, tg
        write(98,*) T(i), rates_lte(i)
    end do
close(98)



END SUBROUTINE
end module subroutines


PROGRAM DissociationRatesH2
use subroutines

implicit none
real*8, allocatable :: T(:)
real*8 :: h
integer i, n, tg, tmax, conv, vi_f
real*8 :: t1, t2
real*8, external :: omp_get_wtime
logical :: input_exists

t1 = omp_get_wtime()

!input file
inquire(file='input',exist=input_exists)
if(.not.input_exists) then
 print*, 'input file does not exist'
 stop 
endif
open(90, file='input')
read(90,*) vi_f
read(90,*) Tmax, tg
read(90,*) conv
close(90)

!Construct temperature grid
allocate (T(tg))
h = Tmax/dble(tg)                !step size
T=0.0d0
do i=1, Tg
    T(i) = i*h
end do


call CalcRates(vi_f,tg,T,conv)

t2 = omp_get_wtime()

print*, 'TIME: ', t2-t1

END PROGRAM
