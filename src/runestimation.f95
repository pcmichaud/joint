! program that gets called to launch estimation
program estimate
    use sp
    implicit none
    call get_command_argument(1,scenario)
    call initsettings
    call initdata
    call initpar
    if(.not.inoestim) then
      call doestimation
    end if
    call postestimation
end program estimate
