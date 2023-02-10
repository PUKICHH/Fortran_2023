program bazaar
use, intrinsic :: iso_c_binding, only: c_ptr, c_null_char, c_null_ptr, &
                         & c_f_pointer, c_char, c_int
  use gdk_pixbuf, only: gdk_pixbuf_get_n_channels, gdk_pixbuf_get_pixels, &
                      & gdk_pixbuf_get_rowstride, gdk_pixbuf_new
  use gtk, only: GDK_COLORSPACE_RGB, FALSE
  use gtk_os_dependent, only: gdk_pixbuf_savev

  implicit none
  
  type(c_ptr) :: my_pixbuf
  ! We use chars because we need unsigned integers:
  character(c_char), dimension(:), pointer :: pixel
  integer(c_int) :: nch, rowstride, pixwidth, pixheight
  integer(c_int) :: cstatus   ! Command status
  double precision, dimension(1:3) :: x, y
  double precision :: xx, yy, diag, r
  integer :: s            ! Triangle vertex number
  integer :: n = 20000   ! Number of points
  integer :: i, p
  
  real :: phi0 = 0
  real :: phiN
  real :: phiN1
  
  real :: v0 = 5
  real :: vN
  real :: vN1
  
  real :: a0
  real :: aN
  real :: aN1
  
  real :: dt = 0.3
  
  integer :: j
  integer(kind=8) :: cycles
  
  real :: gamma = 0.00
  real :: omega = 1

  ! We create a "pixbuffer" to store the pixels of the image.
  ! This pixbuffer has no Alpha channel (15% faster), only RGB.
  ! https://developer.gnome.org/gdk-pixbuf/stable/gdk-pixbuf-The-GdkPixbuf-Structure.html
  pixwidth  = 1000
  pixheight = 800
  my_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8_c_int, &
                           & pixwidth, pixheight)
  nch = gdk_pixbuf_get_n_channels(my_pixbuf)
  rowstride = gdk_pixbuf_get_rowstride(my_pixbuf)
  print *, "Channels= ", nch, "      Rowstride=", rowstride
  call c_f_pointer(gdk_pixbuf_get_pixels(my_pixbuf), pixel, &
                 & (/pixwidth*pixheight*nch/))

  ! The background is black (red=0, green=0, blue=0):
  
  cycles = 2000000
  ! Euler method:
  pixel = char(0)
  j = 0
  phiN = phi0
  vN = v0
  aN = -2 * gamma * v0 - omega**2 * sin(phi0/180*3.1415926535) 
  do while (j < cycles)
    aN = -2 * gamma * vN - omega**2 * sin(phiN/180*3.1415926535) 

    phiN = phiN + vN * dt * 0.01
    vN = vN + aN * dt * 0.01

    
    !print *, j * dt
    print *, phiN
    !print *, vN
    p = 1 + nint(400 + phiN * 6)*nch + nint(400 - vN * 40)*rowstride
    pixel(p + i) = char(255)
    j = j + 1
  end do 
  
  ! Save the picture as a PNG:
  cstatus = gdk_pixbuf_savev(my_pixbuf, "pathE.png"//c_null_char,&
              & "png"//c_null_char, c_null_ptr, c_null_ptr, c_null_ptr)


  ! Euler-Cromer method:
  pixel = char(0)
  j = 0
  phiN = phi0
  vN = v0
  aN = -2 * gamma * v0 - omega**2 * sin(phi0/180*3.1415926535) 
  do while (j < cycles)
    aN = -2 * gamma * vN - omega**2 * sin(phiN/180*3.1415926535) 
    vN = vN + aN * dt * 0.01
    phiN = phiN + vN * dt * 0.01
    
    !print *, j * dt
    print *, phiN
    !print *, vN
    p = 1 + nint(400 + phiN * 6)*nch + nint(400 - vN * 40)*rowstride
    pixel(p + i) = char(255)
    j = j + 1
  end do 
  
  ! Save the picture as a PNG:
  cstatus = gdk_pixbuf_savev(my_pixbuf, "pathEC.png"//c_null_char,&
              & "png"//c_null_char, c_null_ptr, c_null_ptr, c_null_ptr)


  ! Predictor - Corrector Method
  pixel = char(0)
  j = 0
  phiN = phi0
  vN = v0
  aN = -2 * gamma * v0 - omega**2 * sin(phi0/180*3.1415926535) 
  do while (j < cycles)
    aN = -2 * gamma * vN - omega**2 * sin(phiN/180*3.1415926535)
    phiN1 = phiN + vN * dt
    vN1 = vN + aN * dt
    aN1 = -2 * gamma * vN1 - omega**2 * sin(phiN1/180*3.1415926535)
    phiN = phiN + vN * dt + aN * dt**2 / 2 
    vN = vN + (aN + aN1) * dt / 2
    
    !print *, j * dt
    print *, phiN
    !print *, vN
    p = 1 + nint(400 + phiN * 6)*nch + nint(400 - vN * 40)*rowstride
    pixel(p + i) = char(255)
    j = j + 1
  end do 
  
  ! Save the picture as a PNG:
  cstatus = gdk_pixbuf_savev(my_pixbuf, "pathPC.png"//c_null_char,&
              & "png"//c_null_char, c_null_ptr, c_null_ptr, c_null_ptr)
  
   
end program bazaar
