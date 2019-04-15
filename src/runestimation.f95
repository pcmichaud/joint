! program that gets called to launch estimation
program estimate
    use sp
    implicit none
    call get_command_argument(1,scenario) 
    call initsettings
    call initdata
    call initpar
    call doestimation
    call postestimation
end program estimate

