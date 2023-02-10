module plot
  use plplot, PI => PL_PI
  use common_ex1
  
  implicit none
  real :: phi0 = 0
  real :: phiN
  real :: phiN1
  
  real :: v0 = 5
  real :: vN
  real :: vN1
  
  real :: a0
  real :: aN
  real :: aN1
  
  integer :: j
  
  real :: gamma = 0.00
  real :: omega = 1

  real(plflt), allocatable :: x(:), y(:)
  real(plflt) :: xmin, xmax, ymin, ymax
  
  
contains
  subroutine makePlot(xscale, yscale, xoff, yoff, i, dt)
    real(plflt) :: xscale, yscale, xoff, yoff
    real :: dt 
    integer :: i    

    allocate(x(i))
    allocate(y(i))

    ! Call you func here
    ! allocate(e(2,i))

    ! j = 1
    ! phiN = phi0
    ! vN = v0
    ! aN = -2 * gamma * v0 - omega**2 * sin(phi0/180*3.1415926535) 

    ! ymin = (aN**2 / 2 - cos(phi0/180*3.1415926535)) - 1
    ! ymax = (aN**2 / 2 - cos(phi0/180*3.1415926535)) + 1

    ! do while (j <= i)
    !   aN = -2 * gamma * vN - omega**2 * sin(phiN/180*3.1415926535)
    !   vN = vN + aN * dt
    !   phiN = phiN + vN * dt
      
      
    !   !print *, j * dt
    !   print *, phiN
    !   !print *, vN
    !   x(j) = j * dt       
    !   y(j) = (aN**2 / 2 - cos(phiN/180*3.1415926535))
    !   j = j + 1
    ! end do 
    call makeEulerPlot(i, dt)

    xmin = 1 
    xmax = i * dt
    
    !   Set up the viewport and window using PLENV. The range in X is
    !   0.0 to 6.0, and the range in Y is 0.0 to 30.0. The axes are
    !   scaled separately (just = 0), and we just draw a labelled
    !   box (axis = 0).

    call plcol0(1)
    call plenv( xmin, xmax, ymin, ymax, 0, 0 )
    call plcol0(2)
    call pllab( '(time)', '(Energy integral)', 'Energy fluctuation' )

    !   Draw the line through the data
    call plcol0(3)
    call plline( x, y )
    deallocate(x)
    deallocate(y)

  end subroutine makePlot
  subroutine makeEulerPlot(i, dt)
    real :: dt 
    integer :: i    
 
    j = 1
    phiN = phi0
    vN = v0
    aN = -2 * gamma * v0 - omega**2 * sin(phi0/180*3.1415926535) 

    ymin = (aN**2 / 2 - cos(phi0/180*3.1415926535)) - 1
    ymax = (aN**2 / 2 - cos(phi0/180*3.1415926535)) + 1
    do while (j <= i)
      aN = -2 * gamma * vN - omega**2 * sin(phiN/180*3.1415926535)
      
      phiN = phiN + vN * dt
      vN = vN + aN * dt
      
      !print *, j * dt
      print *, phiN
      !print *, vN
      x(j) = j * dt       
      y(j) = (aN**2 / 2 - cos(phiN/180*3.1415926535))
      j = j + 1
    end do 
  endsubroutine makeEulerPlot

  subroutine makeEulerCromerPlot(i,dt)
    real :: dt 
    integer :: i    
 
    j = 1
    phiN = phi0
    vN = v0
    aN = -2 * gamma * v0 - omega**2 * sin(phi0/180*3.1415926535) 

    ymin = (aN**2 / 2 - cos(phi0/180*3.1415926535)) - 1
    ymax = (aN**2 / 2 - cos(phi0/180*3.1415926535)) + 1
    do while (j <= i)
      aN = -2 * gamma * vN - omega**2 * sin(phiN/180*3.1415926535)
      vN = vN + aN * dt
      phiN = phiN + vN * dt

      
      !print *, j * dt
      print *, phiN
      !print *, vN
      x(j) = j * dt       
      y(j) = (aN**2 / 2 - cos(phiN/180*3.1415926535))
      j = j + 1
    end do 
  end subroutine makeEulerCromerPlot
  subroutine makePrColPlot(i,dt)
    real :: dt 
    integer :: i    
 
    j = 1
    phiN = phi0
    vN = v0
    aN = -2 * gamma * v0 - omega**2 * sin(phi0/180*3.1415926535) 

    ymin = (aN**2 / 2 - cos(phi0/180*3.1415926535)) - 1
    ymax = (aN**2 / 2 - cos(phi0/180*3.1415926535)) + 1
    do while (j <= i)
      aN = -2 * gamma * vN - omega**2 * sin(phiN/180*3.1415926535)
      phiN1 = phiN + vN * dt
      vN1 = vN + aN * dt
      aN1 = -2 * gamma * vN1 - omega**2 * sin(phiN1/180*3.1415926535)
      phiN = phiN + vN * dt + aN * dt**2 / 2 
      vN = vN + (aN + aN1) * dt / 2
      
      !print *, j * dt
      print *, phiN
      !print *, vN
      x(j) = j * dt       
      y(j) = (aN**2 / 2 - cos(phiN/180*3.1415926535))
      j = j + 1
    end do 
  end subroutine makePrColPlot
end module plot
