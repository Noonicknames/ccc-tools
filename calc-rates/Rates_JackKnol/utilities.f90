module utilities
contains
  
  subroutine sort_two_arrays(X, Y, N)
    implicit none
    real*8, intent(inout) :: X(0:), Y(0:)
    integer, intent(in) :: N
    integer :: i, j
    real*8 :: a, b

    do i=1, N-1
      a=X(i)
      b=Y(i)
      j=i-1
      do while (j >= 0)
        if(X(j) <= a) exit
        X(j+1) = X(j)
        Y(j+1) = Y(j)
        j=j-1
      enddo !j
      X(j+1)=a
      Y(j+1)=b
    enddo !N
  
  end subroutine sort_two_arrays

  subroutine sort_three_arrays(X, Y, Z, N)
    implicit none
    real*8, intent(inout) :: X(0:), Y(0:), Z(0:)
    integer, intent(in) :: N
    integer :: i, j
    real*8 :: a, b, c

    do i=1, N-1
      a=X(i)
      b=Y(i)
      c=Z(i)
      j=i-1
      do while (j >= 0)
        if(X(j) <= a) exit
        X(j+1) = X(j)
        Y(j+1) = Y(j)
        Z(j+1) = Z(j)
        j=j-1
      enddo !j
      X(j+1)=a
      Y(j+1)=b
      Z(j+1)=c
    enddo !N
  
  end subroutine sort_three_arrays

  subroutine progress(j, message)
  implicit none
  integer, intent(in) ::j
  integer :: k
  character(len=:), allocatable :: bar, clear
  character(*), intent(in) :: message
  character*2 :: spinner
  if(j.lt.0.or.j.gt.100) error stop ' STOPPING in utilities/progress: invalid j'
  bar="[                    ] ???%"

  write(unit=bar(24:26),fmt="(i3)") int(dble(j)/5.0)*5
  do k=1, int(dble(j)/dble(5))
    bar(1+k:1+k)='>'
  enddo
  selectcase(mod(int(dble(j)/5.0),4))
  case (0)
    spinner=' |'
  case(1)
    spinner=' /'
  case(2)
    spinner=' -'
  case(3)
    spinner=' \'
  endselect
  write(unit=6,fmt="(a1,a1,X,a,X,a27,a)") '+', char(13), message, bar, spinner
  if(j.eq.100) then
    call sleepqq(100)
    allocate(character(len(message)+len(bar)+len(spinner)+2) :: clear)
    clear(1:len(clear))=' '
    write(unit=6,fmt="(a1,a1,a)") '+', char(13), clear
    write(unit=6,fmt='(A,A)',advance='no') '+', char(13)
  endif
  return
end subroutine progress

real*8 function integrate(f)
  implicit none
  real*8, intent(in) :: f(0:)
  integer :: iR
  integrate = 0d0
  !!$OMP PARALLEL DO REDUCTION(+:INTEGRATE) PRIVATE(IR) SHARED(f) SCHEDULE(GUIDED)
  do iR=0, size(f)-1
    integrate = integrate + f(iR)
  enddo
  !!$OMP END PARALLEL DO
end function integrate

function readFileInterpolate(fileUnit, grid, opt_in, threshold1, threshold2, i1, i2, order, option)
  !this function reads x and y values from the file specified by unit number
  !-fileUnit, and returns the interpolated/extrapolated values over the
  !-radial grid
  !Author: Liam Scarlett
  !Date modified: 14/05/2017
  use grid_class
  implicit none

!-INPUT-------------------------------------------------------------------------
  integer, intent(in) :: fileUnit !unit number of file to read in.
                                  !-fileUnit should have been opened and will
						    !-be closed by calling program
  type(GridObject), intent(in) :: grid !radial grid object
  integer, optional, intent(in) :: opt_in ! =1 if function is strictly non-negative
  real*8, optional, intent(in) :: threshold1 !R-dependent threshold, zero below
                                            !this point
  real*8, optional, intent(in) :: threshold2 !R-dependent threshold, zero above
                                            !this point
  integer, optional, intent(in) :: order
!-------------------------------------------------------------------------------

!-OUTPUT------------------------------------------------------------------------
  real*8, allocatable :: readFileInterpolate(:) !interp/extrapolated function
  integer, optional, intent(out) :: i1, i2
!-------------------------------------------------------------------------------

!-LOCAL-VARIABLES---------------------------------------------------------------
  real*8, allocatable :: temp_x(:), temp_y(:) !temporary arrays to read file  
                                              !--into
  real*8, allocatable :: x_points(:), y_points(:) !temporary arrays to store
                                              !--x and y points
  integer :: io !iostat while reading fileUnit
  integer :: num_x !number of x points
  integer :: i, j !loop indices
  integer :: target_ind
  real*8 :: lim, power, extrap_coef
  logical :: goto_limit
  character*100 :: label
  real*8, allocatable :: temp(:)
  integer :: opt
  character(len=*), optional, intent(in) :: option
  character(len=:), allocatable :: option_local
!-------------------------------------------------------------------------------
!-EXTERNAL-FUNCTIONS------------------------------------------------------------
 ! real*8 :: interpolate
!-------------------------------------------------------------------------------
  goto_limit = .false.  

  if(present(option)) then
    option_local = option
  else
    option_local = ''
  endif

  if(present(opt_in)) then
    opt = opt_in
  else
    opt = 0
  endif
  select case (opt)
    case(0)
    case (1)
    case default
      stop ' STOPPING in readFileInterpoalte: invalid opt'
  end select

  allocate(readFileInterpolate(0:grid%get_nr()-1))

!-COUNT-LINES-IN-FILE-----------------------------------------------------------
  num_x = 0
  linesLoop: do !loop through lines in file to determine number of x points
    read(fileUnit,*,iostat=io)
    if (io .ne. 0) exit !end of file reached - stop reading
    num_x = num_x + 1
  enddo linesLoop
  do i=0, num_x
    backspace(fileUnit) !rewind fileUnit to read from beginning again
  enddo
!-------------------------------------------------------------------------------
  allocate(temp_x(0:num_x-1), temp_y(0:num_x-1))
  do i=0, num_x-1
    read(fileUnit,*,iostat=io) temp_x(i), temp_y(i)
    if(io.ne.0) then !check if final line in the format "DISS   -1.00000000"
      backspace(fileUnit)
      read(fileUnit,*) label
      if(trim(adjustl(label))=='LIMIT') then
        goto_limit = .true.
        backspace(fileUnit)
        read(fileUnit,*,iostat=io) label, lim, power
        if(io/=0) stop ' Error in utilities%readFileInterpolate: &
          &when using LIMIT on final line of Pot file you must provide &
          &dissociation limit and extrapolation power'
        num_x=num_x-1
        allocate(temp(0:num_x-1))
        temp=temp_x(0:num_x-1)
        deallocate(temp_x)
        allocate(temp_x(0:num_x-1))
        temp_x = temp
        temp=temp_y(0:num_x-1)
        deallocate(temp_y)
        allocate(temp_y(0:num_x-1))
        temp_y = temp
      else
        write(*,'(A)') ' Error in utilities%readFileInterpolate: unknown word "'&
          &//trim(adjustl(label))//'"'
        stop
      endif
    endif
  enddo
  if(present(i1)) then
    i1=0
    do while (i1.le.grid%get_nr()-1)
      if(grid%get_grid_iR(i1).ge.temp_x(0)) exit
      i1=i1+1
    enddo
    if(grid%get_grid_iR(i1).gt.temp_x(0)) i1=i1-1
    if(i1 < 0) i1 = 0
  endif
  if(present(i2)) then
    i2=i1
    target_ind=num_x-1
    do while (temp_x(target_ind).gt.grid%get_grid_iR(grid%get_nr()-1))
      target_ind=target_ind-1
    enddo
    do while (i2.le.grid%get_nr()-1)
      if(grid%get_grid_iR(i2).ge.temp_x(target_ind)) exit
      i2=i2+1
    enddo
    if(i2.gt.grid%get_nr()-1) i2 = i2 - 1
  endif
  if(opt.eq.1.and.sum(temp_y(:)).gt.0.0d0) then
    if(present(threshold1).and..not.present(threshold2)) then
      call shuffle_for_thresholds(temp_x, temp_y, num_x, threshold1=threshold1)
    elseif(present(threshold2).and..not.present(threshold1)) then
      call shuffle_for_thresholds(temp_x, temp_y, num_x, threshold2=threshold2)
    elseif(present(threshold1).and.present(threshold2)) then
      call shuffle_for_thresholds(temp_x, temp_y, num_x, threshold1, threshold2)
    endif
  endif

  if(option_local == 'remove_nuclear') temp_y = temp_y - 1.0d0/temp_x


!INTERP/EXTRAP------------------------------------------------------------------
  if(present(order)) then
    readFileInterpolate(:) = interpolate(temp_x(:), temp_y(:),grid%get_grid(),order=order)
    !readFileInterpolate(:) = mcsplines(temp_x(:), temp_y(:),grid%get_grid())
  else
    readFileInterpolate(:) = interpolate(temp_x(:), temp_y(:),grid%get_grid())
    !readFileInterpolate(:) = mcsplines(temp_x(:), temp_y(:),grid%get_grid())
  endif
  if(goto_limit) then
    power = abs(power)
    extrap_coef = (temp_y(num_x-1)-lim)*temp_x(num_x-1)**power
    do i=0, grid%get_nr()-1
      if (grid%get_grid_iR(i) > temp_x(num_x-1)) then
        readFileInterpolate(i) = extrap_coef/grid%get_grid_iR(i)**power + lim
      endif
    enddo
  endif




!-------------------------------------------------------------------------------
!stop
    do i=0, grid%get_nr()-1
      if((readFileInterpolate(i) .lt. 0 .and. opt .eq. 1)) then
	  readFileInterpolate(i) = 0d0
      endif
      !if(temp_y(0) .lt. 1E-15 .and. grid%get_grid_iR(i) .le. temp_x(0)) then
      !  readFileInterpolate(i) = 0d0
      !endif
    enddo

  if(present(threshold1)) then
    i=0
    do while (grid%get_grid_iR(i) .le. threshold1)
      !force interpolated cross section to be zero for all R <= threshold
      readFileInterpolate(i) = 0d0
      i=i+1
    enddo
  endif

  return
end function readFileInterpolate

subroutine shuffle_for_thresholds(x_points, y_points, num_x_out, threshold1, threshold2)
  implicit none
  real*8, allocatable, intent(inout) :: x_points(:), y_points(:)
  integer, intent(out) :: num_x_out
  integer :: num_x
  real*8, optional, intent(in) :: threshold1, threshold2
  real*8, allocatable :: temp_x(:), temp_y(:)
  integer :: num_before_thresh1, num_after_thresh2, num_CS, extra
  real*8 :: temp
  integer :: i,j

  if(.not.(allocated(x_points).and.allocated(y_points))) then
    stop ' STOPPING in utilities/shuffle_for_thresholds: unallocated input'
  endif
  
    temp=0.0d0
    num_x=size(x_points)
    if(size(y_points).ne.num_x) stop ' STOPPING in utilities/shuffle_for_thresholds: num_x =/= num_y'
    num_before_thresh1=0
    num_after_thresh2=0
    i=0
    do while (i.le.num_x-1.and.temp.eq.0.0d0)
      temp=y_points(i)
      if(temp.eq.0.0d0) num_before_thresh1=num_before_thresh1+1
      i=i+1
    enddo
    i=i-1
    num_CS=0
    do while(i.le.num_x-1.and.temp.ne.0.0d0)
      temp=y_points(i) 
      !print*, '- i, temp', i, temp
      if(temp.ne.0.0d0) then
        num_CS=num_CS+1
     !   print*, 'num_CS++'
      endif
      i=i+1
    enddo
    !print*, 'i, num_x', i, num_x
    i=i-1
    !print*, 'i, temp', i, temp

    do while(i.le.num_x-1.and.temp.eq.0.0d0)
      temp=y_points(i)
      if(temp.eq.0.0d0) num_after_thresh2=num_after_thresh2+1
      i=i+1
    enddo
    if(num_before_thresh1+num_CS+num_after_thresh2.ne.num_x) then
      print*, num_before_thresh1, num_CS, num_after_thresh2, num_x
      stop ' STOPPING in utilities/shuffle_for_threshold:  >2 thresholds'
    endif
    
    extra=0
    if(num_before_thresh1.gt.0.or.present(threshold1)) extra=extra+1
    if(num_after_thresh2.gt.0.or.present(threshold2)) extra=extra+1
    allocate(temp_x(0:size(x_points)-1), temp_y(0:size(y_points)-1))
    temp_x(:) = x_points(:)
    temp_y(:) = y_points(:)
    deallocate(x_points, y_points)
    allocate(x_points(0:num_CS+extra-1), y_points(0:num_CS+extra-1))
    i=0
    if(num_before_thresh1.gt.0.and..not.present(threshold1)) then
      x_points(0)=temp_x(num_before_thresh1-1)
      y_points(0)=0.0d0
      i=i+1
    elseif(present(threshold1)) then
      if(.not.threshold1.lt.temp_x(num_before_thresh1)) then
        x_points(0) = temp_x(num_before_thresh1)-1.0E-5
      else
        x_points(0)=threshold1
      endif
      y_points(0)=0.0d0
      i=i+1
    endif
    do j=0,num_CS-1
      x_points(i)=temp_x(num_before_thresh1+i-1)
      y_points(i)=temp_y(num_before_thresh1+i-1)
      i=i+1
    enddo
    if(num_after_thresh2.gt.0.and..not.present(threshold2)) then
      x_points(num_CS+extra-1)=temp_x(num_before_thresh1+num_CS)
      y_points(num_CS+extra-1)=0.0d0
      i=i+1
    elseif(present(threshold2)) then
      if(.not.threshold2.gt.temp_x(num_before_thresh1+num_CS-1)) then
        x_points(num_CS+extra-1) = temp_x(num_before_thresh1+num_CS-1)+1.0E-5
      else
        x_points(num_CS+extra-1) = threshold2+tiny(threshold2)
        x_points(num_CS+extra-1)=threshold2
      endif
      y_points(num_CS+extra-1)=0.0d0
      i=i+1
    endif

    num_x_out = num_CS+extra
end subroutine shuffle_for_thresholds

function interpolate(xPoints_in, yPoints_in, grid, order, opt)
  !This function returns an array with the function f(xPoints)=yPoints
  !-interpolated and extrapolated over the radial grid.
  !Author: Mark Zammit
  !Modified by Liam Scarlett
  !Date modified: 14/05/2017
  implicit none

!-INPUT-------------------------------------------------------------------------
  real*8, intent(in) :: grid(:) !radial grid
  real*8, intent(in) :: xPoints_in(:), yPoints_in(:) !The function (x,y) values
  integer, optional, intent(in) :: order, opt
!-------------------------------------------------------------------------------
  
!-OUTPUT------------------------------------------------------------------------
  real*8, allocatable :: interpolate(:) !array containing interp/extrapolated
                                           !-function
  real*8, allocatable :: intp_f(:)
!-------------------------------------------------------------------------------
  
!-LOCAL-VARIABLES---------------------------------------------------------------
  real*8 :: dy !required by polint subroutine
  integer :: j !loop index
  integer :: n_order !order of polynomial used for extrapolation
  integer :: num_x !number of xPoints
  real*8, allocatable :: temp_Fx(:), temp_Fy(:) !temp arrays for extrapolation
  real*8 :: m, b
  real*8, allocatable :: xPoints(:), yPoints(:)
  integer :: gridSize
!-------------------------------------------------------------------------------

  gridSize=size(grid)
  allocate(interpolate(gridSize),intp_f(gridSize))

  num_x = size(xPoints_in(:))
  if(size(yPoints_in(:)) .ne. num_x) then
    !number of yPoints should equals number of xPoints
    write(*,*) 'STOPPING IN INTERPOLATE: num_y =/= num_x'
    write(*,*) num_x, size(yPoints_in(:))
    stop
  endif

  allocate(xPoints(num_x), yPoints(num_x))
  xPoints=xPoints_in
  yPoints=yPoints_in
  
  
  if ( num_x >= 3 ) n_order = 3 !use a cubic polynomial for extrapolation,
  if ( num_x == 2 ) n_order = 2 !-unless only 2 points given, then use parabolic.
  if ( num_x <= 1 ) then !can't interp/extrapolate with only 1 point
    write(*,*)& 
      &'STOPPING IN INTERPOLATE: cannot interpolate function with < 2 points'
  endif
  if(present(order)) n_order=min(num_x, 10, order)
!-INTERPOLATE-------------------------------------------------------------------
  intp_f(:) = 0d0
  !the below subroutine will populate interpolate(:) with the interpolated
  !-values over the interval [xPoints(0), xPoints(num_x-1)]
  call intrpl(num_x,xPoints,yPoints,gridSize,grid,&
    &intp_f)
!-------------------------------------------------------------------------------
  m=(yPoints(num_x)-yPoints(num_x-1))/(xPoints(num_x)-xPoints(num_x-1))
  b=yPoints(num_x)-m*xPoints(num_x)

!-EXTRAPOLATE-------------------------------------------------------------------
  allocate( temp_Fx(n_order), temp_Fy(n_order) )
  extrapLoop: do j = 1, gridSize
    if ( grid(j) < xPoints(1) ) then
      !extrapolating over interval [0, xPoints(0) )
      temp_Fx(1:n_order) = xPoints(1:n_order)
      temp_Fy(1:n_order) = yPoints(1:n_order)
    else if ( grid(j) > xPoints(num_x) ) then
      !extrapolating over interval (xPoints(num_x-1), rmax]
      temp_Fx(1:n_order) = xPoints(num_x-n_order+1:num_x)
      temp_Fy(1:n_order) = yPoints(num_x-n_order+1:num_x)
    else
      cycle !don't extrapolate over interval [xPoints(0), xPoints(num_x-1)]
    end if
    !the below subroutine will return the extrapolated value at rgrid(j) by
    !-fitting a polynomial of degree n_order to the first/last n_order number
    !-of yPoints
    !interpolate(j)=m*grid(j)+b
    call  polint(temp_Fx, temp_Fy, n_order, grid(j), intp_f(j), dy)

  end do extrapLoop

  if(present(opt)) then
    if(opt.eq.1) then
      do j=1, gridSize
        if(intp_f(j).lt.0.0d0) intp_f(j) = 0.0d0
      enddo
    endif
  endif
!-------------------------------------------------------------------------------
  interpolate = intp_f 
  return !interpolate(:) now has interpolated and extrapolated values over the
         !-entire radial grid
end function interpolate

subroutine scale_2D(X1, Z1X1X1, X2, Z2X2X2)
  !scales the square surface Z1 to pass through points of Z2
  use Grid_Interpolation
  implicit none
  real*8, intent(in) :: X1(0:)
  real*8, intent(inout) :: Z1X1X1(0:size(X1)-1,0:size(X1)-1)
  real*8, intent(in) :: X2(0:)
  real*8, intent(in) :: Z2X2X2(0:size(X2)-1,0:size(X2)-1)
  real*8 :: Z1X2X2(0:size(X2)-1,0:size(X2)-1)
  real*8 :: Z1X2X1(0:size(X2)-1,0:size(X1)-1)
  real*8 :: RX2X2(0:size(X2)-1,0:size(X2)-1)
  real*8 :: RX2X1(0:size(X2)-1,0:size(X1)-1)
  real*8 :: RX1X1(0:size(X1)-1,0:size(X1)-1)
  integer :: i, j, NX1, NX2, i1, i2, ier

  NX1=size(X1); NX2=size(X2)

  !do i=0, NX1-1
  !  Z1X2X1(:,i)=interpolate(X1, Z1X1X1(:,i), X2)
  !  !Z1X2X1(:,i)=mcsplines(X1, Z1X1X1(:,i), X2)
  !enddo
  !do i=0, NX2-1
  !  Z1X2X2(i,:)=interpolate(X1, Z1X2X1(i,:), X2)
  !  !Z1X2X2(i,:)=mcsplines(X1, Z1X2X1(i,:), X2)
  !enddo

  call bicubic_intrp(1, & !mode
    NX1, & !num x points
    NX1, & !num y points
    X1,  & !x points
    X1,  & !y points
    Z1X1X1,  & !z matrix
    NX2, & !num x interp points
    NX2, & !num y interp points
    X2, & !x interp points
    X2, & !y interp points
    Z1X2X2, & !interpolated matrix
    ier)
    if(ier/=0) error stop ' ERROR in bicubic interpolation for scale_2D'
  
  open(unit=111,file='Z1X1X1.out',action='write',status='replace')
  do i=0,NX1-1
    do j=0,NX1-1
      write(111,*) X1(i),X1(j),Z1X1X1(i,j)
    enddo
    write(111,*)
  enddo
  close(111)
  !open(unit=112,file='Z1X2X1.out',action='write',status='replace')
  !do i=0,NX2-1
  !  do j=0,NX1-1
  !    write(112,*) X2(i),X1(j),Z1X2X1(i,j)
  !  enddo
  !  write(112,*)
  !enddo
  !close(112)
  open(unit=113,file='Z1X2X2.out',action='write',status='replace')
  do i=0,NX2-1
    do j=0,NX2-1
      write(113,*) X2(i),X2(j),Z1X2X2(i,j)
    enddo
    write(113,*)
  enddo
  close(113)

  RX2X2 = Z2X2X2/Z1X2X2
  open(unit=111,file='RX2X2.out',action='write',status='replace')
  do i=0,NX2-1
    do j=0,NX2-1
      write(111,*) X2(i),X2(j),RX2X2(i,j)
    enddo
    write(111,*)
  enddo
  close(111)
  do i=0,NX2-1
    do j=0, NX2-1
      if(Z1X2X2(i,j).eq.0d0) RX2X2(i,j)=0d0
    enddo
  enddo
  !do i=0, NX2-1
  !  RX2X1(i,:)=interpolate(X2, RX2X2(i,:), X1)
  !  !RX2X1(i,:)=mcsplines(X2, RX2X2(i,:), X1)
  !enddo
  !open(unit=111,file='RX2X1.out',action='write',status='replace')
  !do i=0,NX2-1
  !  do j=0,NX1-1
  !    write(111,*) X2(i),X1(j),RX2X1(i,j)
  !  enddo
  !  write(111,*)
  !enddo
  !close(111)
  !i1=0; i2=NX1
  !do i=0, NX1-1
  !  RX1X1(:,i)=interpolate(X2, RX2X1(:,i), X1)
  !  !RX1X1(:,i)=mcsplines(X2, RX2X1(:,i), X1)
  !  if(X1(i)<=X2(0)) i1=i
  !  if(X1(NX1-1-i)>=X2(NX2-1)) i2=NX1-1-i
  !enddo
  
  call bicubic_intrp(1, & !mode
    NX2, & !num x points
    NX2, & !num y points
    X2,  & !x points
    X2,  & !y points
    RX2X2,  & !z matrix
    NX1, & !num x interp points
    NX1, & !num y interp points
    X1, & !x interp points
    X1, & !y interp points
    RX1X1, & !interpolated matrix
    ier)
    if(ier/=0) error stop ' ERROR in 2nd bicubic interpolation for scale_2D'
  
  open(unit=111,file='RX1X1.in',action='write',status='replace')
  do i=0,NX1-1
    do j=0,NX1-1
      write(111,*) X1(i),X1(j),RX1X1(i,j)
    enddo
    write(111,*)
  enddo
  close(111)

  !!! set ratio outside X2*X2 square to constant values

  !X1,X2 <i1 corner:
  RX1X1(0:i1-1,0:i1-1)=RX1X1(i1,i1)
  
  !X1<i1, X2>i2 corner:
  RX1X1(0:i1-1,i2+1:NX1-1)=RX1X1(i1,i2)

  !X1>i2, X2<i1 corner:
  RX1X1(i2+1:NX1-1,0:i1-1)=RX1X1(i2,i1)

  !X1,X2>i2 corner:
  RX1X1(i2+1:NX1-1,i2+1:NX1-1)=RX1X1(i2,i2)

  do i=i1,i2
    !X1<i1, i1<X2<i2 side:
    RX1X1(0:i1-1,i)=RX1X1(i1,i)
    
    !X1>i2, i1<X2<i2 side:
    RX1X1(i2+1:NX1-1,i)=RX1X1(i2,i)

    !i1<X1<i2, X2<i1 side:
    RX1X1(i,0:i1-1)=RX1X1(i,i1)
    
    !i1<X1<i2, X2>i2 side:
    RX1X1(i,i2+1:NX1-1)=RX1X1(i,i2)
  enddo

  do i=0,NX1-1
    do j=0,NX1-1
      if (RX1X1(i,j)<0.0d0) RX1X1(i,j) = 0.0d0
    enddo
  enddo


  open(unit=111,file='RX1X1.out',action='write',status='replace')
  do i=0,NX1-1
    do j=0,NX1-1
      write(111,*) X1(i),X1(j),RX1X1(i,j)
    enddo
    write(111,*)
  enddo
  close(111)

  Z1X1X1=Z1X1X1*RX1X1
end subroutine scale_2D

subroutine scale_2D_rect(X1, Y1, Z1X1Y1, X2, Y2, Z2X2Y2)
  !scales the square surface Z1 to pass through points of Z2
  use Grid_Interpolation
  implicit none
  real*8, intent(in) :: X1(0:), Y1(0:)
  real*8, intent(inout) :: Z1X1Y1(0:size(X1)-1,0:size(Y1)-1)
  real*8, intent(in) :: X2(0:), Y2(0:)
  real*8, intent(in) :: Z2X2Y2(0:size(X2)-1,0:size(Y2)-1)
  real*8 :: Z1X2Y2(0:size(X2)-1,0:size(Y2)-1)
  real*8 :: RX2Y2(0:size(X2)-1,0:size(Y2)-1)
  real*8 :: RX1Y1(0:size(X1)-1,0:size(Y1)-1)
  integer :: i, j, NX1, NX2, NY1, NY2, i1, i2, ier
  integer :: iX1,iX2, iY1, iY2

  NX1=size(X1); NX2=size(X2)
  NY1=size(Y1); NY2=size(Y2)

  call bicubic_intrp(1, & !mode
    NX1, & !num x points
    NY1, & !num y points
    X1,  & !x points
    Y1,  & !y points
    Z1X1Y1,  & !z matrix
    NX2, & !num x interp points
    NY2, & !num y interp points
    X2, & !x interp points
    Y2, & !y interp points
    Z1X2Y2, & !interpolated matrix
    ier)
    if(ier/=0) error stop ' ERROR in bicubic interpolation for scale_2D'
  
  RX2Y2 = Z2X2Y2/Z1X2Y2
  
  call bicubic_intrp(1, & !mode
    NX2, & !num x points
    NY2, & !num y points
    X2,  & !x points
    Y2,  & !y points
    RX2Y2,  & !z matrix
    NX1, & !num x interp points
    NY1, & !num y interp points
    X1, & !x interp points
    Y1, & !y interp points
    RX1Y1, & !interpolated matrix
    ier)
    if(ier/=0) error stop ' ERROR in 2nd bicubic interpolation for scale_2D'
  

  !!! set ratio outside X2*Y2 square to constant values
  iX1=0; iX2=NX1-1
  iY1=0; iY1=NY1-1
  do i=0,NX1-1
    if(X1(i)<=X2(0)) iX1=i
    if(X1(NX1-1-i)>=X2(NX2-1)) iX2=NX1-1-i
  enddo
  do j=0,NY1-1
    if(Y1(j)<=Y2(0)) iY1=j
    if(Y1(NY1-1-j)>=Y2(NY2-1)) iY2=NY1-1-j
  enddo

  !X1,Y1 <iY1 corner:
  if(iX1>0.and.iY1>0) RX1Y1(0:iX1-1,0:iY1-1)=RX1Y1(iX1,iY1)
  
  !X1<iX1, Y1>iY2 corner:
  if(iX1>0.and.iY2<NY1-1) RX1Y1(0:iX1-1,iY2+1:NY1-1)=RX1Y1(iX1,iY2)

  !X1>iX2, Y1<iY1 corner:
  if(iX2<NX1-1.and.iY1>0) RX1Y1(iX2+1:NX1-1,0:iY1-1)=RX1Y1(iX2,iY1)

  !X1,Y1>iY2 corner:
  if(iX2<NX1-1.and.iY2<NY1-1) RX1Y1(iX2+1:NX1-1,iY2+1:NY1-1)=RX1Y1(iX2,iY2)

  do i=iX1,iX2
    !iX1<X1<iX2, Y1<iY1 side:
    RX1Y1(i,0:iY1-1)=RX1Y1(i,iY1)
    
    !iX1<X1<iX2, Y1>iY2 side:
    RX1Y1(i,iY2+1:NY1-1)=RX1Y1(i,iY2)
  enddo
  do j=iY1,iY2
    !X1<iX1, iY1<Y1<Y2 side:
    RX1Y1(0:iX1-1,j)=RX1Y1(iX1,j)
    
    !X1>iX2, iY1<Y1<Y2 side:
    RX1Y1(iX2+1:NX1-1,j)=RX1Y1(iX2,j)
  enddo

  do i=0,NX1-1
    do j=0,NY1-1
      if (RX1Y1(i,j)<0.0d0) RX1Y1(i,j) = 0.0d0
    enddo
  enddo

  open(unit=111,file='RX1Y1.out',action='write',status='replace')
  do i=0,NX1-1
    do j=0,NY1-1
      write(111,*) X1(i),Y1(j),RX1Y1(i,j)
    enddo
    write(111,*)
  enddo
  close(111)

  Z1X1Y1=Z1X1Y1*RX1Y1

end subroutine scale_2D_rect

subroutine scale_2D_rect_old(X1, Y1, Z1X1Y1, X2, Y2, Z2X2Y2)
  !scales the rectangular surface Z1 to pass through points of Z2
  implicit none
  real*8, intent(in) :: X1(0:), Y1(0:)
  real*8, intent(inout) :: Z1X1Y1(0:size(X1)-1,0:size(Y1)-1)
  real*8, intent(in) :: X2(0:), Y2(0:)
  real*8, intent(in) :: Z2X2Y2(0:size(X2)-1,0:size(Y2)-1)
  real*8 :: Z1X2Y2(0:size(X2)-1,0:size(Y2)-1)
  real*8 :: Z1X2Y1(0:size(X2)-1,0:size(Y1)-1)
  real*8 :: RX2Y2(0:size(X2)-1,0:size(Y2)-1)
  real*8 :: RX2Y1(0:size(X2)-1,0:size(Y1)-1)
  real*8 :: RX1Y1(0:size(X1)-1,0:size(Y1)-1)
  integer :: i, j, NX1, NX2, NY1, NY2, iX1, iX2, iY1, iY2

  NX1=size(X1); NX2=size(X2)
  NY1=size(Y1); NY2=size(Y2)

  do i=0, NY1-1
    Z1X2Y1(:,i)=interpolate(X1, Z1X1Y1(:,i), X2)
  enddo
  do i=0, NX2-1
    Z1X2Y2(i,:)=interpolate(Y1, Z1X2Y1(i,:), Y2)
  enddo
  
  open(unit=111,file='Z1X1Y1.out',action='write',status='replace')
  do i=0,NX1-1
    do j=0,NY1-1
      write(111,*) X1(i),Y1(j),Z1X1Y1(i,j)
    enddo
    write(111,*)
  enddo
  close(111)
  open(unit=112,file='Z1X2Y1.out',action='write',status='replace')
  do i=0,NX2-1
    do j=0,NY1-1
      write(112,*) X2(i),Y1(j),Z1X2Y1(i,j)
    enddo
    write(112,*)
  enddo
  close(112)
  open(unit=113,file='Z1X2Y2.out',action='write',status='replace')
  do i=0,NX2-1
    do j=0,NY2-1
      write(113,*) X2(i),Y2(j),Z1X2Y2(i,j)
    enddo
    write(113,*)
  enddo
  close(113)

  RX2Y2 = Z2X2Y2/Z1X2Y2
  open(unit=111,file='RX2Y2.out',action='write',status='replace')
  do i=0,NX2-1
    do j=0,NY2-1
      write(111,*) X2(i),Y2(j),RX2Y2(i,j)
    enddo
    write(111,*)
  enddo
  close(111)
  do i=0,NX2-1
    do j=0, NY2-1
      if(Z1X2Y2(i,j).eq.0d0) RX2Y2(i,j)=0d0
    enddo
  enddo
  do i=0, NX2-1
    RX2Y1(i,:)=interpolate(Y2, RX2Y2(i,:), Y1)
  enddo
  open(unit=111,file='RX2Y1.out',action='write',status='replace')
  do i=0,NX2-1
    do j=0,NY1-1
      write(111,*) X2(i),Y1(j),RX2Y1(i,j)
    enddo
    write(111,*)
  enddo
  close(111)
  do i=0, NY1-1
    RX1Y1(:,i)=interpolate(X2, RX2Y1(:,i), X1)
  enddo
    
  open(unit=111,file='RX1Y1.in',action='write',status='replace')
  do i=0,NX1-1
    do j=0,NY1-1
      write(111,*) X1(i),Y1(j),RX1Y1(i,j)
    enddo
    write(111,*)
  enddo
  close(111)

  !!! set ratio outside X2*Y2 square to constant values
  
  iX1=0; iX2=NX1-1
  iY1=0; iY1=NY1-1
  do i=0,NX1-1
    if(X1(i)<=X2(0)) iX1=i
    if(X1(NX1-1-i)>=X2(NX2-1)) iX2=NX1-1-i
  enddo
  do j=0,NY1-1
    if(Y1(j)<=Y2(0)) iY1=j
    if(Y1(NY1-1-j)>=Y2(NY2-1)) iY2=NY1-1-j
  enddo

  !X1,Y1 <iY1 corner:
  if(iX1>0.and.iY1>0) RX1Y1(0:iX1-1,0:iY1-1)=RX1Y1(iX1,iY1)
  
  !X1<iX1, Y1>iY2 corner:
  if(iX1>0.and.iY2<NY1-1) RX1Y1(0:iX1-1,iY2+1:NY1-1)=RX1Y1(iX1,iY2)

  !X1>iX2, Y1<iY1 corner:
  if(iX2<NX1-1.and.iY1>0) RX1Y1(iX2+1:NX1-1,0:iY1-1)=RX1Y1(iX2,iY1)

  !X1,Y1>iY2 corner:
  if(iX2<NX1-1.and.iY2<NY1-1) RX1Y1(iX2+1:NX1-1,iY2+1:NY1-1)=RX1Y1(iX2,iY2)

  do i=iX1,iX2
    !iX1<X1<iX2, Y1<iY1 side:
    RX1Y1(i,0:iY1-1)=RX1Y1(i,iY1)
    
    !iX1<X1<iX2, Y1>iY2 side:
    RX1Y1(i,iY2+1:NY1-1)=RX1Y1(i,iY2)
  enddo
  do j=iY1,iY2
    !X1<iX1, iY1<Y1<Y2 side:
    RX1Y1(0:iX1-1,j)=RX1Y1(iX1,j)
    
    !X1>iX2, iY1<Y1<Y2 side:
    RX1Y1(iX2+1:NX1-1,j)=RX1Y1(iX2,j)
  enddo

  do i=0,NX1-1
    do j=0,NY1-1
      if (RX1Y1(i,j)<0.0d0) RX1Y1(i,j) = 0.0d0
    enddo
  enddo

  open(unit=111,file='RX1Y1.out',action='write',status='replace')
  do i=0,NX1-1
    do j=0,NY1-1
      write(111,*) X1(i),Y1(j),RX1Y1(i,j)
    enddo
    write(111,*)
  enddo
  close(111)

  Z1X1Y1=Z1X1Y1*RX1Y1
end subroutine scale_2D_rect_old

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

!-=-= READ_PEC_FILE =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
subroutine read_PEC_file(filename, grid, f)
  use grid_class
  implicit none

  !-=-= INPUT
  character(len=*), intent(in) :: filename
  type(GridObject), intent(in) :: grid !radial grid object


  !-=-= OUTPUT
  real(rp), dimension(grid%get_nr()) :: f !the interpolated function

  !-=-= LOCAL VARIABLES
  real(rp), dimension(:), allocatable :: temp_x, temp_x2, temp_y, temp_y2 !temporary arrays to read file into
  integer :: io !iostat while reading nunit
  integer :: num_x, nr !number of x/y points
  integer :: i, iR1, iR2 !loop indices
  real(rp) :: l1, l2, C1, C2, p1, p2, limit
  character(len=100) :: label
  real(rp), allocatable :: temp(:)
  logical :: goto_limit
  real(rp), dimension(:), allocatable :: Dy
  real(rp) :: R1, R2, fR1, fR2, DfR1, DfR2
  integer :: nunit, exists, nunit2
  logical :: remove_nuclear = .true.

  inquire(file=filename,exist=exists)
  if(.not. exists) then
    write(*,'(A)') '***ERROR: Cannot find PEC file '//filename
    error stop
  endif

  open(newunit=nunit,file=filename,action='read')

  nr = grid%get_nr()

  goto_limit = .false.  

  !Count number of lines in file
  num_x = 0
  do !loop through lines in file to determine number of x points
    read(nunit,*,iostat=io)
    if (io .ne. 0) exit !end of file reached - stop reading
    num_x = num_x + 1
  enddo

  do i=0, num_x
    backspace(nunit) !rewind nunit to read from beginning again
  enddo
 
  !Read the file
  allocate(temp_x(num_x), temp_y(num_x))
  do i=1, num_x

    read(nunit,*,iostat=io) temp_x(i), temp_y(i)
    if(io.ne.0) then !check if final line in the format "LIMIT   -1.00000000"
      backspace(nunit)
      read(nunit,*) label

      if(trim(adjustl(label))=='LIMIT') then
        goto_limit = .true.
        backspace(nunit)
        read(nunit,*,iostat=io) label, limit
        if(io/=0) stop ' ERROR in read_file_interpolate: &
          &when using LIMIT on final line of file you must provide a limit and extrapolation power'
        num_x=num_x-1
        allocate(temp(num_x))
        temp=temp_x(1:num_x)
        deallocate(temp_x)
        allocate(temp_x(num_x))
        temp_x = temp
        temp=temp_y(1:num_x)
        deallocate(temp_y)
        allocate(temp_y(num_x))
        temp_y = temp
        deallocate(temp)
      else

        write(*,'(A)') '*** ERROR in read_file_interpolate: unknown word "'//trim(adjustl(label))//'"'
        error stop

      endif !WORD
    endif !io
    if(i > 1 .and. i <= num_x) then
      if(temp_y(i) == temp_y(i-1)) then
        write(*,'(A)') '*** ERROR in read_file_interpolate: identical Y values'
        write(*,'("    At line ",I0,":")') i
        write(*,*) temp_x(i-1), temp_y(i-1)
        write(*,*) temp_x(i), temp_y(i)
        error stop
      endif
    endif
  enddo
    
  !Remove 1/R nuclear term 
  if(remove_nuclear) then
    allocate(temp_x2(num_x+2),temp_y2(num_x+2), Dy(num_x+2))

    !We only want to remove the 1/R term for R <= 1.0.
    !First we find the closest point (iR1) to R=1.0 in the fine grid:
    do iR1=1, nr
      if(grid%get_grid_iR(iR1) >= 1.0d0) exit
    enddo
    R1 = grid%get_grid_iR(iR1)
    R2 = grid%get_grid_iR(iR1+1)

    !Now we find the first point (iR2) after 1.0 in the input grid:
    do iR2=1, num_x
      if(temp_x(iR2) > 1.0d0) exit
    enddo

    !Now we insert R1 and R2 at the iR2 position of the input grid:
    temp_x2(1:iR2-1) = temp_x(1:iR2-1)
    temp_x2(iR2) = R1
    temp_x2(iR2+1) = R2
    temp_x2(iR2+2:num_x+2) = temp_x(iR2:num_x)

    !And interpolate the input function onto the new grid:
    call intrpl(num_x, temp_x,temp_y, num_x+2, temp_x2, temp_y2, Dy)
    deallocate(Dy)
    
    !Now remove the 1/R term from the input function
    temp_y2(1:iR2) = temp_y2(1:iR2) - 1.0d0/temp_x2(1:iR2)

    !And interpolate up to iR1:
    allocate(Dy(nr))
    call intrpl(iR2, temp_x2(1:iR2),temp_y2(1:iR2), iR1, grid%get_grid(0,iR1-1), f(1:iR1), Dy(1:iR1))

    !Now interpolate from iR1+1 to nr:
    call intrpl(num_x-iR2+2, temp_x2(iR2+1:num_x+2),temp_y2(iR2+1:num_x+2), nr-iR1, grid%get_grid(iR1,nr-1), f(iR1+1:), Dy(iR1+1:))
    
    deallocate(temp_x2, temp_y2)
    
    !Now replace the 1/R term:

    f(1:iR1) = f(1:iR1) + 1.0d0/grid%get_grid(0,iR1-1)

  else

    !Interpolate without removing 1/R term:
    allocate(Dy(nr))
    call intrpl(num_x, temp_x,temp_y, nr, grid%get_grid(), f, Dy)

  endif !remove_nuclear

  !Extrapolate to small/large R
  if(goto_limit) then
    
    !For small R: PEC(R) = 1/R + C*R + l
    !Find the first value of R in the range of temp_x
    do i=1,nr
      if(grid%get_grid_iR(i-1,i-1) > temp_x(1)) exit
    enddo
    i = i - 1
    
    !Now determine the fitting coefficients C and l.
    !We have :
    !
    !   f(R) = 1/R + C*R + l
    !   f'(R) = -/R**2 + C
    !
    !and solving for p and C gives:
    !
    !   C = f'(R) + 1/R**2
    !   l = f(R) - C*R - 1/R
    
    !We evaluate C and l at the point R(i): (the Dy array contains the derivative)
    R1 = grid%get_grid_iR(i-1,i-1)
    fR1 = f(i)
    DfR1 = Dy(i)
    
    if(remove_nuclear) then
      C1 = DfR1
      DfR1 = DfR1 - 1.0d0/R1**2
    else
      C1 = DfR1 + 1.0d0/R1**2
    endif
    l1 = fR1 - C1*R1 - 1.0d0/R1
    p1 = 1.0d0
    
    do i=1, nr
      if (grid%get_grid_iR(i-1,i-1) >= temp_x(1)) exit
      f(i) = 1.0d0/grid%get_grid_iR(i-1,i-1) + C1*grid%get_grid_iR(i-1,i-1) + l1
    enddo

    !For large R: PEC(R) = C/R**p + l    
    !Find the last value of R in the range of temp_x
    do i=1,nr
      if(grid%get_grid_iR(i-1) > temp_x(num_x)) exit
    enddo
    i = i - 1

    !Now determine the fitting coefficients C and p.
    !We have :
    !
    !   f(R) = C*R**p + l
    !   f'(R) = p * C*R**(p-1)
    !
    !and solving for p and C gives:
    !
    !   p = R * f'(R) / (f(R) - l)
    !   C = (f(R) - l) / R**p
    
    !We evaluate C and p at the point R(i): (the Dy array contains the derivative)
    R2 = grid%get_grid_iR(i-1)
    DfR2 = Dy(i)
    fR2 = f(i)
    l2 = limit
    p2 = R2 * DfR2 / (fR2 - l2)
    C2 = (fR2 - l2) / R2**p2

    do i=nr, 1, -1
      if (grid%get_grid_iR(i-1) <= temp_x(num_x)) exit
      f(i) = C2*grid%get_grid_iR(i-1)**(p2) + l2
    enddo

    write(*,*)
    write(*,'("  Interpolating/extrapolating PEC file ",A)') filename

    write(*,*)
    if(R2 >= 100.0d0) then
    write(*,'("    =-= EXTRAP PARAMETERS FOR R < ",F3.1," =-=-= EXTRAP PARAMETERS FOR R > ",F5.1," =-=")') R1,R2
    else
    write(*,'("    =-= EXTRAP PARAMETERS FOR R < ",F3.1," =-=-= EXTRAP PARAMETERS FOR R > ",F4.1," -=-=")') R1,R2
    endif
    write(*,'("    |   f(R) = 1/R + C*R**p + l",7X,      "  |   f(R) = C*R**p + l",17X,             "|")')
    write(*,'("    |      C = ",ES12.5,11X,              "  |      C = ",ES12.5,15X,                "|")'), C1, C2
    write(*,'("    |      p = ",ES12.5,11X,              "  |      p = ",ES12.5,15X,                "|")'), p1, p2
    write(*,'("    |      l = ",ES12.5,11X,              "  |      l = ",ES12.5,15X,                "|")'), l1, l2
    if(R2 >= 100.0d0) then
    write(*,'("    |   Fitted at R = ",F3.1,13X,         "  |   Fitted at R = ",F5.1,15X,           "|")') R1, R2
    else
    write(*,'("    |   Fitted at R = ",F3.1,13X,         "  |   Fitted at R = ",F4.1,16X,           "|")') R1, R2
    endif
    write(*,'("    |   to match: f(R) = ",ES12.5,1X,     "  |   to match: f(R) = ",ES12.5,5X,       "|")') fR1, fR2
    write(*,'("    |      (d/dR) f(R) = ",ES12.5,1X,     "  |      (d/dR) f(R) = ",ES12.5,5X,       "|")') DfR1, DfR2
    write(*,'("    =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-",    "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=")')
    write(*,*)

    !Check that extrapolated function has correct limit.
    ! - If something has gone wrong calculating the extrapolation parameters
    !   then C2 and p could end up very small and numerical error will cause PEC(R)
    !   to diverge at large R
    if(abs(f(nr)-limit) > grid%get_expcut()) then
      write(*,'(A)') '*** ERROR in read_file_interpolate: extrapolated PEC diverges at large R'
      print*, 'PEC:', f(nr)
      print*, 'LIMIT:', limit
      print*, 'DIFF:', abs(f(nr)-limit)
      error stop
    endif

  endif

  deallocate(temp_x,temp_y)

end subroutine read_PEC_file

end module utilities
