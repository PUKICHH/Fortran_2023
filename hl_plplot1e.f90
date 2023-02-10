! Copyright (C) 2011
! Free Software Foundation, Inc.
!
! This file is part of the gtk-fortran gtk+ Fortran Interface library.
!
! This is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 3, or (at your option)
! any later version.
!
! This software is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! Under Section 7 of GPL version 3, you are granted additional
! permissions described in the GCC Runtime Library Exception, version
! 3.1, as published by the Free Software Foundation.
!
! You should have received a copy of the GNU General Public License along with
! this program; see the files LICENSE and LICENSE_EXCEPTION respectively.
! If not, see <http://www.gnu.org/licenses/>.
!------------------------------------------------------------------------------
! Contributed by: James Tappin
! PLplot code derived from PLplot's example 1 by Alan W. Irwin
! Last modifications: vmagnin 2020-06-10 (GTK 4), 2022-04-20
!------------------------------------------------------------------------------

module common_ex1
  use, intrinsic :: iso_c_binding      ! Enable the c-binding routines & constants
  ! These are the gtk & glib routines used explicitly in the code:
  use gtk, only: gtk_widget_show, gtk_window_set_child, gtk_window_destroy
  ! These are the high-level drawing area modules:
  use gtk_draw_hl
  ! This makes the low-level pl_cmd routine accessible:
  use plplot_extra

  ! The size of the drawing area:
  integer(c_int) :: height, width
  ! The top-level window must be here as its destroy signal need not come from it:
  type(c_ptr) :: window
end module common_ex1

module plplot_code_ex1
  use plplot, PI => PL_PI
  use common_ex1
  use plot
  implicit none
  real(plflt) :: xscale, yscale, xoff, yoff
contains
  subroutine x01f95(area)

    type(c_ptr), intent(in) :: area
    type(c_ptr) :: cc
    character(len=80) :: version
    character(len=20) :: geometry
    integer :: digmax, cycles
    real :: deltaT
    ! needed for use as functions instead of subroutines
    integer :: plparseopts_rc
    integer :: plsetopt_rc

    ! Define colour map 0 to match the "GRAFFER" colour table in
    ! place of the PLPLOT default.
    integer, parameter, dimension(16) :: &
      & rval = [255, 0, 255, 0, 0, 0, 255, 255, 255, 127, 0, 0, 127, 255, 85, 170],&
      & gval = [ 255, 0, 0, 255, 0, 255, 0, 255, 127, 255, 255, 127, 0, 0, 85, 170],&
      & bval = [ 255, 0, 0, 0, 255, 255, 255, 0, 0, 0, 127, 255, 255, 127, 85, 170]

    !  Process command-line arguments
    plparseopts_rc = plparseopts(PL_PARSE_FULL)
    if (plparseopts_rc /= 0) stop "plparseopts error"

    !  Print plplot version
    call plgver(version)
    write (*,'(a,a)') 'PLplot library version: ', trim(version)

    ! Get a cairo context from the drawing area.
    cc = hl_gtk_drawing_area_cairo_new(area)

    !  Initialize plplot
    call plscmap0(rval, gval, bval)
    call plsdev("extcairo")

    ! By default the "extcairo" driver does not reset the background
    ! This is equivalent to the command line option "-drvopt set_background=1"
    plsetopt_rc = plsetopt("drvopt", "set_background=1")
    if (plsetopt_rc /= 0) stop "plsetopt error"

    ! The "extcairo" device doesn't read the size from the context.
    write(geometry, "(I0,'x',I0)") width, height
    plsetopt_rc = plsetopt( 'geometry', geometry)
    if (plsetopt_rc /= 0) stop "plsetopt error"

    !  Divide page into 2x2 plots
    call plstar(2,2)

    ! Tell the "extcairo" driver where the context is located. This must be
    ! done AFTER the plstar or plinit call.
    call pl_cmd(PLESC_DEVINIT, cc)

    !  Set up the data
    !  Original case
    xscale = 6._plflt
    yscale = 1._plflt
    xoff = 0._plflt
    yoff = 0._plflt

    !  Do a plot
    print *, 'Please enter quantity of iterations: '
    read(*,*) cycles
    print *, 'Please enter time interval: '
    read(*,*) deltaT

    call makePlot(xscale, yscale, xoff, yoff, cycles, deltaT)
    call makePlot(xscale, yscale, xoff, yoff, cycles / 10, deltaT * 10)

    !  Do a plot
    digmax = 5
    call plsyax(digmax,  0)
    ! call plot(xscale, yscale, xoff, yoff)

    !  Don't forget to call PLEND to finish off, and then delete the
    !  cairo context.
    call plend()
    call hl_gtk_drawing_area_cairo_destroy(cc)

  end subroutine x01f95

  !======================================================================
  
end module plplot_code_ex1

module handlers_ex1
  use common_Ex1
  use gtk_hl_container
  use gtk_hl_button
  use gtk_draw_hl
  use, intrinsic :: iso_c_binding

  implicit none
  integer(c_int) :: run_status = TRUE
  real(c_double), parameter :: pi = acos(-1.0_c_double)

contains

  subroutine quit_cb(widget, gdata) bind(c)
    type(c_ptr), value, intent(in) :: widget, gdata
    call gtk_window_destroy(window)
  end subroutine quit_cb

  subroutine activate(app, gdata) bind(c)
    ! This gives the main program access to the plotting code
    use plplot_code_ex1
    use gtk, only: gtk_application_window_new, gtk_window_set_title
    implicit none
    type(c_ptr), value, intent(in)  :: app, gdata
    ! Pointers toward our GTK widgets:
    type(c_ptr) :: drawing, scroll_w, base, qbut

    ! Set the size of the drawing area (these are global variables)
    height = 1000
    width = 1200

    ! Create a top-level window and then pack a column box into it:
    window = gtk_application_window_new(app)
    call gtk_window_set_title(window, "PLplot x01 / gtk-fortran (extcairo)"//c_null_char)

    base = hl_gtk_box_new()
    call gtk_window_set_child(window, base)

    ! Create a drawing area, in a 600x500 scrolled window.
    ! The high-level drawing area creator automatically adds a cairo surface as
    ! backing store. Here we use the default expose/draw callback which
    ! just copies the backing store to the drawing surface.
    ! Pack it into the vertical box.
    drawing = hl_gtk_drawing_area_new(size=[width, height], &
         & has_alpha = FALSE, &
         & scroll = scroll_w, &
         & ssize=[ 600, 500 ])
    call hl_gtk_box_pack(base, scroll_w)

    ! Add a quit button, and pack that into the box as well:
    qbut = hl_gtk_button_new("Quit"//c_null_char, clicked=c_funloc(quit_cb))
    call hl_gtk_box_pack(base, qbut, expand=FALSE)

    ! Display the widgets:
    call gtk_widget_show(window)

    ! Call the plotting routine:
    call x01f95(drawing)
  end subroutine activate
end module handlers_ex1


program cairo_plplot_ex1
  use, intrinsic :: iso_c_binding, only: c_ptr, c_funloc, c_null_char
  ! Set the size of the drawing area (these are global variables)
  use handlers_ex1
  use gtk_hl_container, only: hl_gtk_application_new
  implicit none
  type(c_ptr) :: my_app

  ! Initalize GTK, create the GUI and launch the main loop:
  my_app = hl_gtk_application_new("gtk-fortran.plplot.hl_plplot1e"//c_null_char, &
                             & c_funloc(activate))
end program cairo_plplot_ex1
