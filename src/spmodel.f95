module sp
! ------------------------------------------------------------
! Stated-preference for couples - Michaud and van Soest (2011)
! f95 module to estimate parameters of model
! Data created with ExportFortran.do (in alp/codes)
! using LAPACK and BLAS and with dcdflib (for statistical dist.)
! compiled with gfortran sp2.f95 dcdflib.a -llapack -lblas -o sp2
! Last modifications: September 12th 2010
! ------------------------------------------------------------

implicit none
! FIXED PARAMETERS
integer Time			! maximum age to which can survive
double precision Lmax;	! maximum amount of leisure in a week (hours)
integer nd				! number of draws in simulating likelihood, 1 if no sim
integer nm				! possible number of ratings (1 to 10)
integer nparmax 		! maximum number of parameters
parameter (Lmax =168.0d0, Time = 100, nm = 10, nparmax = 200)

! size of arrays
integer nvar	! number of variables in dataset
integer nj		! number of questions
integer nr  	! number of ratings
integer nc		! number of choice questions
integer n		! number of records
integer nxm		! number of taste shifters husband
integer nxf		! number of taste shifters wife
integer nz		! number of distribution factors (weight)
integer npar	! number of parameters
integer nfreepar ! number of free parameters

! estimation path
character(len=50) estpath
		
! DATA declared as global (dimensions allocated in inidata once info read)
integer, allocatable :: Id(:)				! id of respondent (n)
double precision, allocatable :: Xm(:,:)			! taste shifters husband (n,nxm)
double precision, allocatable :: Xf(:,:)			! taste shifters wife (n,nxf)
double precision, allocatable :: Z(:,:)				! weight factors (n,nz)
integer, allocatable :: R(:,:,:)					! ratings (n,nr,2)
integer, allocatable :: C(:,:,:)					! choices (n,nc,3)
integer, allocatable :: age(:,:)					! age of respondents (n,2)
integer, allocatable ::	Who(:)						! who answered question (n)
double precision, allocatable :: Hm(:,:,:,:)		! hours of husbands in scenarios (n,nj,2,3) 
double precision, allocatable :: Hf(:,:,:,:)		! hours of wives in scenarios (n,nj,2,3)
double precision, allocatable :: Y(:,:,:,:)			! household income in scenarios  (n,nj,2,3)
double precision, allocatable :: D(:, :, :)			! for uniform draws (n,nd,2)
character*20, allocatable :: shifters(:)                ! for variable labels of shifters
double precision, allocatable :: L(:,:,:)
double precision, allocatable :: pHL(:,:), sHL(:,:), p75r(:,:)
double precision gomp(2,2)
double precision, allocatable :: wages(:,:), hours(:,:)
double precision, allocatable :: Hm_sim(:,:,:,:)		! hours of husbands in simulations
double precision, allocatable :: Hf_sim(:,:,:,:)		! hours of wives in simulations
double precision, allocatable :: Y_sim(:,:,:,:)			! household income in simulations
double precision, allocatable :: value(:,:,:)			! expected utility for simulations
double precision, allocatable :: cprob(:,:,:)
! scenarios and settings
character*50 scenario
logical isurvival, iload, includeratings, icorr, iestimate, ihetero, icomplement, iunitary, idiscount, idirect
character*50 initfile, data, info, survival
double precision arf, drc, reprate
logical ishufheter, ishufwages, iblockcomp

! life table survival probabilities (age 1 to 100)

! INTERMEDIATE ARRAYS for parameters
double precision, allocatable :: freepar(:)	, allpar(:) 	! array to store free parameter values
double precision fixpar(nparmax)
character(len=25) labpar(nparmax)	
character(len=25), allocatable :: labfreepar(:)						
													! array to fix parameters (unbounded = 999.0d0)
! list of subroutines
contains

	! INIT SETTING
	subroutine initsettings
		character*10 buffer
		open(1,file='../params/settings_'//adjustl(trim(scenario))//'.info')
		read(1,*) buffer, isurvival
		read(1,*) buffer, iload
		read(1,*) buffer, iestimate
		read(1,*) buffer, data
		read(1,*) buffer, info
		read(1,*) buffer, includeratings
		read(1,*) buffer, icomplement
		read(1,*) buffer, ihetero
		read(1,*) buffer, icorr
		read(1,*) buffer, iunitary
		read(1,*) buffer, idiscount
		read(1,*) buffer, idirect
		read(1,*) buffer, arf
		read(1,*) buffer, drc
		read(1,*) buffer, reprate
		read(1,*) buffer, ishufheter
		read(1,*) buffer, ishufwages
		read(1,*) buffer, iblockcomp
		close(1)

		if (.not. ihetero) then
			nd = 1
		else
			nd = 50
		end if 

	end subroutine initsettings

	subroutine set_random_seed
	  implicit none
	  integer, allocatable :: seed(:)
	  integer :: n, i
	  call random_seed(size = n)
	  allocate(seed(n))
	  	do i = 1, n, 1
	  		seed(i) = i
		end do
	   call random_seed(put=seed)
	end subroutine set_random_seed


	! LOAD DATA
	subroutine initdata
		double precision, allocatable :: buf(:)						
		double precision draw
		double precision alpha, beta, pm, pf
		integer pos, s, u, t, i, iid
		! Load info file to get sizes
		write(*,*) '+ read information'
		open(1,file='../data/'//adjustl(trim(info)))	
				read(1,*) 
                        read(1,*) nj,nr,nc,n,nxm,nxf,nz,nvar
                close(1)

                nxm = nxm + 1
                nxf = nxf + 1
                nz  = nz + 1
                allocate(shifters(nxm))
                open(2,file='../data/varlist_ref.dat')
                read(2,*)
                shifters(1) = 'constant'
                do i = 2, nxm, 1
                   read(2,*) shifters(i)
                end do   
                close(2)
                		
		! Report on dataset
		write(*,*) '-------------------------------------'
		write(*,*) 'Information of dataset to load'
		write(*,'(A,I4)') 'number of scenarios = ', nj
		write(*,'(A,I4)') 'number of ratings = ', nr
		write(*,'(A,I4)') 'number of choice = ', nc
		write(*,'(A,I4)') 'number of records = ', n
		write(*,'(A,I4)') 'number of shifters m (incl const) = ', nxm
		write(*,'(A,I4)') 'number of shifters f (incl const) = ', nxf
		write(*,'(A,I4)') 'number of distr. factors (incl const) = ', nz
		write(*,'(A,I4)') 'number of vars dataset = ', nvar
		write(*,*) '-------------------------------------'
				
		! Allocate global arrays using info on sizes
		allocate(buf(nvar-1))
		allocate(Id(n))
		allocate(Xm(n,nxm))
		allocate(Xf(n,nxf))
		allocate(Z(n,nz))
		allocate(R(n,nr,2))
		allocate(C(n,nc,3))
		allocate(age(n,2))
		allocate(Who(n))
		allocate(Hm(n,nj,2,3))
		allocate(Hf(n,nj,2,3))
		allocate(Y(n,nj,2,3))
		allocate(D(n,nd,2))
		allocate(pHL(n,4))
		allocate(sHL(n,4))		
		allocate(p75r(n,2))
		allocate(wages(n,2))
		allocate(hours(n,2))

		! Load data

		open(2,file='../data/'//adjustl(trim(data)))
        
        do i= 1, n, 1
           pos = 1
           ! read entire record
		   read(2,*) iid,buf
		   ! distribute record
		   	! id
		   	Id(i) = iid
		   	! taste shifters husband
		   	Xm(i,:) = [1.0d0,buf(pos:pos+nxm-2)]
		   	pos = pos + nxm-1
		   	! taste shifters wife
		   	Xf(i,:) = [1.0d0,buf(pos:pos+nxf-2)]
		   	pos = pos + nxf-1
		   	! weight factors
		   	Z(i,:) = [1.0d0,buf(pos:pos + nz -2)]
		   	pos = pos + nz-1
		   	! ratings
		   	do s = 1, nr, 1
		   		R(i,s,:) = nint(buf(pos:pos + 1))
		   		pos = pos + 2
		   	end do
		    ! choices 	
		   	do s = 1, nc, 1
		   		C(i,s,:) = nint(buf(pos:pos + 2))
		   		pos = pos + 3
		   	end do
		   	! age
		   	age(i,:) = nint(buf(pos:pos+1))
		   	pos = pos + 2
		   	! who
		   	who(i) = nint(buf(pos))
		   	pos = pos + 1
		   	! Hours husband
			do s = 1, nj, 1
				if (s.le.nr) then
					Hm(i,s,1,:) = buf(pos:pos+2)
					!write(*,*) Hm(i,s,1,:)
					pos = pos + 3
				else 
					Hm(i,s,1,:) = buf(pos:pos+2)
					pos = pos + 3
					Hm(i,s,2,:) = buf(pos:pos+2)
					pos = pos + 3
				end if
			end do
		   	! Hours wife
			do s = 1, nj, 1
				if (s.le.nr) then
					Hf(i,s,1,:) = buf(pos:pos+2)
					pos = pos + 3
				else 
					Hf(i,s,1,:) = buf(pos:pos+2)
					pos = pos + 3
					Hf(i,s,2,:) = buf(pos:pos+2)
					pos = pos + 3
				end if
			end do
			! Income
			do s = 1, nj, 1
				if (s.le.nr) then
					Y(i,s,1,:) = buf(pos:pos+2)
					pos = pos + 3
				else 
					Y(i,s,1,:) = buf(pos:pos+2)
					pos = pos + 3
					Y(i,s,2,:) = buf(pos:pos+2)
					pos = pos + 3
				end if
			end do	
			! reading health limit work probabilities
			pm = buf(pos)
			pos = pos + 1
			pf = buf(pos)
			pos = pos + 1

			pHL(i,1) = (1.0d0 - pm)*(1.0d0 - pf)
			pHL(i,2) = pm*(1.0d0 - pf)
			pHL(i,3) = (1.0d0 - pm)*pf
			pHL(i,4) = pm*pf

			sHL(1,:) = (/0.0,0.0/)
			sHL(2,:) = (/1.0,0.0/)
			sHL(3,:) = (/0.0,1.0/)
			sHL(4,:) = (/1.0,1.0/)

			p75r(i,1) = buf(pos)
			pos = pos + 1
			p75r(i,2) = buf(pos)
			pos = pos + 1

			wages(i,1) = buf(pos)
			pos = pos + 1
			wages(i,2) = buf(pos)
			pos = pos + 1
			hours(i,1) = buf(pos)
			pos = pos + 1
			hours(i,2) = buf(pos)
		end do
		close(2)

		! Taking uniform draws for both dimensions
		if (ihetero) then
			call set_random_seed
			do i = 1, n, 1
				do u = 1, nd, 1
					call random_number(draw)

					D(i,u,1) = quann(draw)
					if (D(i,u,1).lt.-10.0d0) then
						D(i,u,1) = -10.0d0
					else if (D(i,u,1).gt.10.0d0) then
						D(i,u,1) = 10.0d0
					end if
					if (nd.eq.1) then
						D(i,u,1) = 0.0d0
					end if	
					call random_number(draw)	
					D(i,u,2) = quann(draw)
					if (D(i,u,2).lt.-10.0d0) then
						D(i,u,2) = -10.0d0
					else if (D(i,u,2).gt.10.0d0) then
						D(i,u,2) = 10.0d0
					end if
					if (nd.eq.1) then
						D(i,u,2) = 0.0d0
					end if	
	!			  print *, i, u,  D(i,u,:)
				end do	
			end do
		else
			D(:,:,:) = 0.0d0
		end if	
		! load life tables
		open(3,file='../params/gompertz_ref.csv')
			read(3,*) gomp(1,:)
			read(3,*) gomp(2,:)
		close(3)
		allocate(L(2,n,Time))
		if (isurvival) then
			do i = 1, n, 1
				do s = 1, 2
					alpha = gomp(2,s)
					beta = gomp(1,s)
					alpha = p75r(i,s)*alpha
					do t = 1, Time, 1
						L(s,i,t) = dexp(-alpha/beta *(dexp(beta*dble(t))-1.0d0))							
					end do
				end do
			end do
		else
			L(:,:,:) = 1.0d0
		end if
	end subroutine initdata
	
	
	! INITIALIZE PARAMETERS
	! allpar is npar by 1, freepar is nfreepar by 1
	! fixpar is what controls the parameters that are being fixed
	! just uncomment fixpar(pos) line when you want to fix
	subroutine initpar
		integer pos, i, m, j, nentry
		! temporary array to store initial values, nparmax = 200
		double precision ipar(nparmax), epar(nparmax), temp
		character*80 buffer
		! whether to use existing parameters

		! by default set unbounded (to value 999.0d0)
		do i = 1, nparmax, 1
			fixpar(i) = 999.0d0
		end do

		! load parameters
		if (iload) then
			open(5, file='../params/parms_'//adjustl(trim(scenario))//'.csv')
			read(5,*) temp
			read(5,*) nentry
			do i = 1, nentry, 1
				read(5,*) buffer, epar(i), temp
				!write(*,*) epar(i)
            end do
            close(5)
         else
          if (trim(scenario) .eq. 'ref') then
        	else
				open(5, file='../params/parms_ref.csv')
				read(5,*) temp
				read(5,*) nentry
				do i = 1, nentry, 1
					read(5,*) buffer, epar(i), temp
					!write(*,*) epar(i)
	            end do
	            close(5)

        	end if
         end if

		! initialize parameter values
		pos = 1
			! HUSBAND UTILITY
			! husband consumption (exp(a))
			ipar(pos) = 0.1d0
			labpar(pos) = 'alpha_c'
			!fixpar(pos) = ipar(pos)
			pos = pos + 1
			! marginal utility of leisure husband
			ipar(pos) = 0.2d0
			labpar(pos) = 'alpha_lm_cons'
			!fixpar(pos) = ipar(pos)
			pos = pos + 1
			do i = 2, nxm, 1
				ipar(pos) = 0.0d0
				labpar(pos) = 'alpha_lm_'//adjustl(trim(shifters(i)))
				!fixpar(pos) = ipar(pos)
				pos = pos + 1
			end do
			!Below when running genders separately.
            !fixpar(pos-1) = ipar(pos-1)

			! husband wife's leisure
			ipar(pos) = 0.0d0
			labpar(pos) = 'alpha_lf'
			if (.not. idirect) then
				fixpar(pos) = ipar(pos)
			end if	
			pos = pos + 1
			! husband wife's leisure complementarity
			ipar(pos) = 0.0d0
			labpar(pos) = 'alpha_lm_lf'
			if (.not. icomplement) then
				fixpar(pos) = ipar(pos)
			end if	
			!fixpar(pos) = ipar(pos)
			pos = pos + 1		


			! WIFE UTILITY
			! wife consumption
			ipar(pos) = 0.1d0
			labpar(pos) = 'beta_c'
			!fixpar(pos) = ipar(pos)
			pos = pos + 1
			! marginal utility of leisure wife
			ipar(pos) = 0.2d0
			labpar(pos) = 'beta_lf_cons'
			pos = pos + 1
			do i = 2, nxf, 1
				ipar(pos) = 0.0d0
				labpar(pos) = 'beta_lf_'//adjustl(trim(shifters(i)))
				pos = pos + 1
			end do


			! wife husband's leisure
			ipar(pos) = 0.0d0
			labpar(pos) = 'beta_lm'
			if (.not. idirect) then
				fixpar(pos) = ipar(pos)
			end if	
			pos = pos + 1
			! wife husband's leisure complementarity
			ipar(pos) = 0.0d0
			labpar(pos) = 'beta_lm_lf'
			if (.not. icomplement) then
				fixpar(pos) = ipar(pos)
			end if	
			pos = pos + 1	
			! WEIGHT
				ipar(pos) = 0.0d0
				labpar(pos) = 'mu'
				!fixpar(pos) = ipar(pos)
				pos = pos + 1
			do i = 2, nz, 1
				ipar(pos) = 0.0d0
				labpar(pos) = 'wageratio'
				if (iunitary) then
					fixpar(pos) = ipar(pos)
				end if	
				pos = pos + 1
			end do
			! DISCOUNT FACTORS 
			ipar(pos)   = dlog(0.95d0)
			labpar(pos) = 'log_rho_m'
			if (.not. idiscount) then
				fixpar(pos) = ipar(pos)
			end if	
			ipar(pos+1) = dlog(0.95d0)
			labpar(pos+1) = 'log_rho_f'
			if (.not. idiscount) then
				fixpar(pos+1) = ipar(pos+1)
			end if
			pos = pos + 2
			! THRESHOLDS HUSBAND
			! first threshold estimated (no constant in utility)
			! tau(m) = tau(m-1) + exp(g(m))
			ipar(pos) = -3.0d0
			labpar(pos) = 'tau_m_0'
			!	fixpar(pos) = ipar(pos)
			if(.not. includeratings) then
			   fixpar(pos) = ipar(pos)
			end if
			pos = pos + 1
			do m = 2, nm-1, 1
				ipar(pos) = 1.0d0
				labpar(pos) = 'log_tau_m'
			    if(.not. includeratings) then
			      fixpar(pos) = ipar(pos)
			    end if
				pos = pos + 1
			end do
			! THRESHOLDS WIFE
			! first threshold estimated (no constant in utility)
			! tau(m) = tau(m-1) + exp(g(m))
			ipar(pos) = -3.0d0
			labpar(pos) = 'tau_f_0'
			!	fixpar(pos) = ipar(pos)
			if(.not. includeratings) then
			   fixpar(pos) = ipar(pos)
			end if
			pos = pos + 1
			do m = 2, nm-1, 1
				ipar(pos) = 1.0d0
				labpar(pos) = 'log_tau_f'
			!	fixpar(pos) = ipar(pos)
			    if(.not. includeratings) then
                  fixpar(pos) = ipar(pos)
                end if
				pos = pos + 1
			end do

			! EXCESS VARIANCE RATINGS HUSBAND (sig = exp(sig*))
			ipar(pos) = dlog(1.0d0)
			labpar(pos) = 'log_sig_m'
			
			if(.not. includeratings) then
                  fixpar(pos) = ipar(pos)
            end if
			pos = pos + 1
			! EXCESS VARIANCE RATINGS WIFE (sig = exp(sig*))
			ipar(pos) = dlog(1.0d0)
			labpar(pos) = 'log_sig_f'
			
			if(.not. includeratings) then
                  fixpar(pos) = ipar(pos)
            end if
			pos = pos + 1
			! UH terms (choleski terms for heterogeneity in leisure)
			ipar(pos)   = 1.0d0	! L(nm,nm)
			labpar(pos) = 'L_nm_nm'
			if (.not. ihetero) then
				fixpar(pos) = 0.0d0
			end if	
			ipar(pos+1) = 0.0d0 	! L(nf,nm)
			labpar(pos+1) = 'L_nf_nm'
			if (.not. ihetero) then
				fixpar(pos+1) = ipar(pos+1)
			end if	
			if (.not. icorr) then
				fixpar(pos+1) = ipar(pos+1)
			end if	
			ipar(pos+2) = 1.0d0	! L(nf,nf)
			labpar(pos+2) = 'L_nf_nf'
			if (.not. ihetero) then
				fixpar(pos+2) = 0.0d0
			end if	
			pos = pos + 3
			
			! Scale for the utility function
            ipar(pos) = 1.0d0
            labpar(pos) = 'scale_m'
            fixpar(pos) = ipar(pos)
            pos = pos + 1
            ipar(pos) = 1.0d0
            labpar(pos) = 'scale_f'
            fixpar(pos) = ipar(pos)
            pos = pos + 1
			ipar(pos) = 1.0d0
            labpar(pos) = 'scale_h'
            fixpar(pos) = ipar(pos)
            pos = pos + 1



			! total number of parameters
			npar = pos-1
				
			! count number of free parameters	
			nfreepar = 0
			do i = 1, npar, 1
				if (fixpar(i) .eq. 999.0d0) then
					nfreepar = nfreepar + 1
				end if	
			end do
			! now transfer ipar() to freepar(), allocate memory for freepar()	
			allocate(freepar(nfreepar))
			allocate(labfreepar(nfreepar))
			j = 1
			do i = 1, npar, 1
				if (fixpar(i) .eq. 999.0d0) then
					if (iload) then
						freepar(j) = epar(i)
					else
						freepar(j) = ipar(i)
					end if	
					labfreepar(j) = labpar(i)
					j = j+1
				end if	
			end do
			
			write(*,*) 'initial values of parameters'
			do i=1,nfreepar,1
				write(*,'(I4, A, F9.3)') i, labfreepar(i),freepar(i)
			end do
			
	end subroutine initpar
	! GET ALL PARAMETERS FROM FREEPARS
	! function to get all parameters from freepar, plugs in fixed values
	subroutine getpar(freebeta, beta)
		double precision freebeta(nfreepar), beta(npar)
		integer i, j
		j = 1
		do i = 1, npar, 1
			if (fixpar(i) .eq. 999.0d0) then
				beta(i) = freebeta(j)
				j = j+1
			else
				beta(i) = fixpar(i)
			end if
		end do
	end subroutine
	
	! MAPPING PARAMETER VECTOR TO INDIVIDUAL PARAMETERS
	! parsing the parameter vector into useful parameter arrays for computations
	subroutine maptopar(par,bm,bf,bz, rho, cutm, cutf, sigm, sigf, Lo, utilscale)
		double precision par(npar), bm(4,nxm), bf(4,nxf), bz(nz) 
		double precision rho(2), cutm(nm+1), cutf(nm+1), sigm, sigf, Lo(2,2), utilscale(3)
		integer pos, i, m
		pos = 1
		! initialize parameter values
			! HUSBAND UTILITY
			! husband consumption
			bm(1,1) = par(pos)
			pos = pos + 1
			! marginal utility of leisure husband
			do i = 1, nxm, 1
				bm(2,i) = par(pos)
				pos = pos + 1
			end do
			! husband wife's leisure
			bm(3,1) = par(pos)
			pos = pos + 1
			! husband wife's leisure complementarity
			bm(4,1) = par(pos)
			pos = pos + 1
			
			! WIFE UTILITY
			! wife consumption
			bf(1,1) = par(pos)
			pos = pos + 1
			! marginal utility of leisure wife
			do i = 1, nxf, 1
				bf(2,i) = par(pos)
				pos = pos + 1
			end do
			! wife husband's leisure
			bf(3,1) = par(pos)
			pos = pos + 1
			! wife husband's leisure complementarity
			bf(4,1) = par(pos)
			pos = pos + 1
			
			! WEIGHT
			do i = 1, nz, 1
				bz(i) = par(pos)
				pos = pos + 1
			end do
			
			! DISCOUNT FACTORS
			rho(1) = dexp(par(pos))
			rho(2) = dexp(par(pos+1))
			pos = pos + 2
			! THRESHOLDS HUSBAND (pad with -.Inf and +.Inf, here =1.0d6)
			cutm(1) = -1.0d6
			cutm(2) = par(pos)
			pos = pos + 1
			do m = 3, nm, 1
				cutm(m) = cutm(m-1) + dexp(par(pos))
				pos = pos + 1
			end do			
			cutm(nm+1) = 1.0d6
			! THRESHOLDS WIFE (pad with -.Inf and +.Inf, here =1.0d6)
			cutf(1) = -1.0d6
			cutf(2) = par(pos)
			pos = pos + 1
			do m = 3, nm, 1
				cutf(m) = cutf(m-1) + dexp(par(pos))
				pos = pos + 1
			end do		
			cutf(nm+1) = 1.0d6
			! EXCESS VARIANCE RATINGS HUSBAND
			sigm = dexp(par(pos))
			pos = pos + 1
			! EXCESS VARIANCE RATINGS WIFE
			sigf = dexp(par(pos))
			pos = pos + 1
			! UH terms (choleski terms)
			Lo(1,1) = par(pos)    ! L(nm,nm)
			Lo(2,1) = par(pos+1)  ! L(nf,nm)
			Lo(2,2) = par(pos+2)  ! L(nf,nf)
			Lo(1,2) = 0.0d0       ! L(nm,nf)
			pos = pos + 3

			! Scale for the utility function
			utilscale(1) = par(pos)
			utilscale(2) = par(pos+1)
			utilscale(3) = par(pos+2)
			pos = pos+2

			if (pos .ne. npar) then 
				write(*,*) 'warning, not all parameters have been assigned!',pos, npar
			end if			
	end subroutine maptopar
	
	! COMPUTING DISCOUNT FACTORS FOR 3 SUB-PERIODS
	! here assuming only individual survival maters to each spouse
	subroutine factor(i, am, af, who, rho, fm, ff, agem, agef)
		integer am, af, who, refa, a, agem(3), agef(3), i
		double precision rho(2), fm(3), ff(3)
			fm(1) = 0.0d0
			fm(2) = 0.0d0
			fm(3) = 0.0d0
			do a = am, Time, 1
					if (who.eq.1) then
						refa = a
					else
						refa = a + (af-am)
					end if
					if (refa.ge.62 .and. refa.lt.65) then
						fm(1) = fm(1) + (L(1,i,a)/L(1,i,am))*(rho(1)**(a-am)) 
					else if (refa.ge.65 .and. refa.lt.68) then
						fm(2) = fm(2) + (L(1,i,a)/L(1,i,am))*(rho(1)**(a-am)) 
					else if (refa.ge.68) then
						fm(3) = fm(3) + (L(1,i,a)/L(1,i,am))*(rho(1)**(a-am))
					end if
					
			end do
			! wife
			ff(1) = 0.0d0
			ff(2) = 0.0d0
			ff(3) = 0.0d0
			do a = af, Time, 1
					if (who.eq.0) then
						refa = a
					else
						refa = a + (am-af)
					end if
					if (refa.ge.62 .and. refa.lt.65) then
						ff(1) = ff(1) + (L(2,i,a)/L(2,i,af))*(rho(2)**(a-af)) 
					else if (refa.ge.65 .and. refa.lt.68) then
						ff(2) = ff(2) + (L(2,i,a)/L(2,i,af))*(rho(2)**(a-af)) 
					else if (refa.ge.68) then
						ff(3) = ff(3) + (L(2,i,a)/L(2,i,af))*(rho(2)**(a-af))
					end if
			end do
			
			
			if (who.eq.0) then
			    agef(1) = 62
			    agef(2) = 65
			    agef(3) = 68
			    
			    agem(1) = 62 + (am - af)
			    agem(2) = 65 + (am - af)
			    agem(3) = 68 + (am - af)
			    
			else
			    agem(1) = 62
			    agem(2) = 65
			    agem(3) = 68
			    
			    agef(1) = 62 + (af - am)
			    agef(2) = 65 + (af - am)
			    agef(3) = 68 + (af - am)			    
			end if
			
		
	end subroutine factor
	
	pure subroutine getutility(cons, lown, lsp, parc,parown, parsp, parlmlf, eta, utility)
        double precision, intent(in)  ::lown, lsp, cons, parc, parown, parsp, parlmlf,  eta
        double precision, intent(out) ::utility

        utility = parc*dlog(cons)  + (parown+eta)*dlog(lown) + parsp*dlog(lsp) &
            + parlmlf*dlog(lsp)*dlog(lown)

	end subroutine getutility
	
	
	! to compute ordered probit probabilities
	subroutine getp_order(choice, v, cut, nanswers, sig, prob)
			integer nanswers, choice
			double precision v, cut(nanswers+1), prob, sig, high, low
			
			high = probl((cut(choice+1) - v)/dsqrt(sig))
			low =  probl((cut(choice) - v)/dsqrt(sig))
			prob = high - low
			!write(*,*) v, high , low, choice, cut(choice+1), cut(choice)
	end subroutine getp_order
	
	! computing array of probabilities p for n respondents
	subroutine getprob(freebeta, prob)
		double precision freebeta(nfreepar), beta(npar), prob(n)
		! integers
		integer i,u,j, s, k, agem(3), agef(3),hh
		! intermediate arrays double
		double precision fm(3), ff(3), w, lf,lm,cons,v,vm,vf,um,uf,vm1,vf1,vm2,vf2,vh1,vh2
		double precision  pf, ph, pu, pi, pm
		! arrays for parameters
		double precision bm(4,nxm), bf(4,nxf), bz(nz), bum, buf, buma, bufa
		double precision rho(2),cutm(nm+1), cutf(nm+1), sigm, sigf, Lo(2,2), uscale(3)
	


		! get all parameters from freebeta array and fixpar array
		call getpar(freebeta, beta)
		! map parameters
		call maptopar(beta, bm, bf, bz, rho, cutm, cutf, sigm, sigf, Lo, uscale)
		! start loop over respondents

		do i = 1, n, 1	
			! compute discount factors (same across questions)
			call factor(i,age(i,1), age(i,2) , who(i), rho, fm, ff, agem, agef)
		
			! compute weight (same across questions)
			! initialize weight
			w = 0.0d0
			do j = 1, nz, 1
				w = w + Z(i,j)*bz(j)
			end do
			if (w .ge. 5.0d0) then
				w = 1.0d0
			else if (w .le. -5.0d0) then
				w = 0.0d0
			else 	
				w = dexp(w)/(1.0d0+dexp(w))
			end if
	
			
			! reduce marginal utility of leisure into bum and buf
			bum = bm(2,1)
			do j = 2, nxm, 1
				bum = bum + Xm(i,j)*bm(2,j)
			end do	
			buf = bf(2,1)
			do j = 2, nxf, 1
				buf = buf + Xf(i,j)*bf(2,j)
			end do	
			
			! initialize probability for respondent i
			pi = 0.0d0
			! start looping over draws for UH
			do u = 1, nd, 1
				! update draws for UH
				um = Lo(1,1)*D(i,u,1) 
				uf = Lo(2,1)*D(i,u,1) + Lo(2,2)*D(i,u,2)
				! initialize probability p(i,u)
				pu = 1.0d0
				! start looping over questions, first nr are ratings, remaining choices
				do j = 1, nj, 1

					! change nr here if you want to consider only subset of questions
			
					if (j.le.nr) then			! RATINGS QUESTIONS
						if (includeratings) then
							! there are two responses for each rating, husband and wife
							
							! HUSBAND
							! initialize husband's expected utility
							vm = 0.0d0
							! loop over three time intervals in scenario
							do s = 1, 3, 1
							    ! add current age to marginal utility
	                            buma = bum  + bm(2,2)*(dble(agem(s)-62)- Xm(i,2))
								! compute log own leisure
								lm = Lmax - Hm(i,j,1,s)
								! compute other spouse leisure (normalized between 0 and 1)
								lf = Lmax - Hf(i,j,1,s)
								! compute log consumption
								cons = Y(i,j,1,s)
								! compute utility for this period
							    call getutility(cons, lm, lf, bm(1,1), buma, bm(3,1), bm(4,1), um, v)
								! discount and add to expected utility
								vm = vm + fm(s)*v
							end do
							! get probability of rating husband
							if (R(i,j,1) .lt. 99) then
								call getp_order(R(i,j,1), vm, cutm, nm, sigm, pm)
							else 
								pm = 1.0d0
							end if
							! WIFE
							! initialize wife's expected utility
							vf = 0.0d0
							! loop over three time intervals in scenario
							do s = 1, 3, 1
							    ! add current age to marginal utility
							    bufa = buf + bf(2,2)*(dble(agef(s)-62)-Xf(i,2)) 
								! compute log own leisure
								lf = Lmax - Hf(i,j,1,s)
								! compute other spouse leisure (normalized between 0 and 1)
								lm = Lmax - Hm(i,j,1,s)
								! compute log consumption
								cons = Y(i,j,1,s)
								! compute utility for this period
								call getutility(cons, lf, lm, bf(1,1), bufa, bf(3,1), bf(4,1), uf, v)
								
								! discount and add
								vf = vf + ff(s)*v
							end do
							! get probability of rating wife
							if (R(i,j,2) .lt. 99) then
								call getp_order(R(i,j,2), vf, cutf, nm, sigf, pf)
							else
								pf = 1.0d0
							end if	
							! compute product over j for given draw u and individual i
							pu = pu*pm*pf
						end if
					else  							! CHOICE QUESTIONS
						
						! index of choice question
						k = j - nr
						! there are three responses per choice question (husband, wife, household)
						! for each question, there are two scenarios (options)
						! use following to change nc if want only subset of choice questions
						if (k.le.nc) then

						vm1 = 0.0d0
						vm2 = 0.0d0
						vf1 = 0.0d0
						vf2 = 0.0d0

						do hh = 1, 4, 1

							! HUSBAND
							! expected utility option 1
							
							do s = 1, 3, 1
							    ! add current age to marginal utility

							    buma = bum + bm(2,2)*(dble(agem(s)-62)-Xm(i,2))
							    ! make change for health limitations
							    buma = buma + bm(2,3)*(sHL(hh,1) - Xm(i,3))
								lm = Lmax - Hm(i,j,1,s)
								lf = Lmax - Hf(i,j,1,s)
								cons = Y(i,j,1,s)
								call getutility(cons, lm, lf, bm(1,1), buma, bm(3,1), bm(4,1), um, v)
								vm1 = vm1 + pHL(i,hh)*fm(s)*v
							end do
							! expected utility option 2
							
							do s = 1, 3, 1
							     ! add current age to marginal utility
							    buma = bum + bm(2,2)*(dble(agem(s)-62)-Xm(i,2)) 
							    ! make change for health limitations
							    buma = buma + bm(2,3)*(sHL(hh,1) - Xm(i,3))							    
								lm = Lmax - Hm(i,j,2,s)
								lf = Lmax - Hf(i,j,2,s)
								cons = Y(i,j,2,s)
								call getutility(cons, lm, lf, bm(1,1), buma, bm(3,1), bm(4,1), um, v)
								vm2 = vm2 + pHL(i,hh)*fm(s)*v
							end do

							! WIFE
							! expected utility option 1
							
							do s = 1, 3, 1
							    ! add current age to marginal utility
							    bufa = buf + bf(2,2)*(dble(agef(s)-62)-Xf(i,2)) 
							    ! make change for health limitations
							    bufa = bufa + bf(2,3)*(sHL(hh,2) - Xf(i,3))							    
								lf = Lmax - Hf(i,j,1,s)
								lm = Lmax - Hm(i,j,1,s)
								cons = Y(i,j,1,s)
								call getutility(cons, lf, lm, bf(1,1), bufa, bf(3,1), bf(4,1), uf, v)
								vf1 = vf1 + pHL(i,hh)*ff(s)*v
							end do
							! expected utility option 2
							
							do s = 1, 3, 1
							    ! add current age to marginal utility
							    bufa = buf + bf(2,2)*(dble(agef(s)-62)-Xf(i,2)) 
							    ! make change for health limitations
							    bufa = bufa + bf(2,3)*(sHL(hh,2) - Xf(i,3))							    
								lf = Lmax - Hf(i,j,2,s)
								lm = Lmax - Hm(i,j,2,s)
								cons = Y(i,j,2,s)
								call getutility(cons, lf, lm, bf(1,1), bufa, bf(3,1), bf(4,1), uf, v)
								vf2 = vf2 + pHL(i,hh)*ff(s)*v
							end do		

						end do
						
						! HOUSEHOLD
						! expected utility household option 1
						vh1 = w*vm1 + (1.0d0-w)*vf1
						! expected utility household option 2
						vh2 = w*vm2 + (1.0d0-w)*vf2


						! probability observing choice
						if (C(i,k,1) .eq. 1) then
							pm =  probl((vm1 - vm2)/uscale(1))
						else if (C(i,k,1) .eq. 2) then
							pm = probl((vm2 - vm1)/uscale(1))
						else 
						    pm = 1.0d0	
						end if

						! probability observing choice
						if (C(i,k,2) .eq. 1) then
							pf = probl((vf1-vf2)/uscale(2))
						else if (C(i,k,2) .eq. 2) then
							pf = probl((vf2-vf1)/uscale(2))
						else
						    pf = 1.0d0	
						end if
	
						! probability observing choice option 
						if (C(i,k,3).eq.1) then
							ph = probl((vh1-vh2)/uscale(3))
						else if (C(i,k,3) .eq. 2) then
							ph = probl((vh2-vh1)/uscale(3))
						else 
						    ph = 1.0d0	
						end if
						
						! product over j for given u						
						pu = pu*pm*pf*ph	
						end if
					end if ! end if statement over question type

				end do ! end loop questions over questions
			! sum over draws for respondent i
			pi = pi + pu
			end do ! end loop over draws	
			! probability of respondent i (average over p(i,u))
			prob(i) = pi/dble(nd)
			if (prob(i) .lt. 1.0d-35) then
				prob(i) = 1.0d-35
			end if	
			!write(*,*) prob
		end do ! end loop respondents	
		
	end subroutine getprob
	
	! PERFORM ESTIMATION
	subroutine doestimation
		implicit none
		integer i, j, m, iter, ii
		double precision epsrf, prob(n), freebeta(nfreepar), func
		double precision gradient(nfreepar), hessian(nfreepar,nfreepar), invhessian(nfreepar,nfreepar)
		double precision beta_up(nfreepar), step, beta(npar)
		double precision prob_up(n), dprob(n,nfreepar) 
		logical converged, feasible

        ! Jumping to neiboring values to check robustness.
        double precision   :: optim, best
        logical,parameter  :: nudge = .false.
        integer, parameter :: maxnudge = 0   !!! These are when you converge at a given value
        integer            :: nbnudge


        optim   = -huge(1d0)
        best    = -huge(1d0)
        nbnudge = 0


		freebeta = freepar
		! step for forward difference
		epsrf = 1.0d-6
   		! convergence indicator
		converged = .false.
		feasible = .true.
		write(*,*) ''
		! Iteration Loop
		iter = 0


		if (iestimate) then	
			do while ((.not. converged))
				
				! get probabilities (i=1,...,n) at current value of parameters
				call getprob(freebeta,prob)	
				do i = 1, n, 1
				!	write(*,*) prob(i)
				end do
				! initialize arrays for function, gradient and hessian
				func = 0.0d0
				do i = 1,nfreepar
					gradient(i) = 0.0d0
					do j=1,nfreepar
						hessian(i,j) = 0.0d0
					end do
				end do

				! get numerical derivatives of probabilities (n by npar)
		
				do j = 1,nfreepar,1
					! forward evaluation
					beta_up = freebeta
					beta_up(j) = freebeta(j) + epsrf
					call getprob(beta_up,prob_up)
					do i = 1, n, 1
						! numerical gradient of p
						dprob(i,j) = (prob_up(i) - prob(i))/(epsrf)
					end do
				end do
				! compute gradient, hessian (BHHH) and function
				do i=1, n, 1
					! add likelihood contribution for i
					func = func + dlog(prob(i))
					! compute gradient and hessian (dlog(pi)
					do j=1,nfreepar,1
						gradient(j) = gradient(j) + (dprob(i,j)/prob(i))
						do m = 1, nfreepar, 1
							hessian(j,m) = hessian(j,m) + (dprob(i,j)/prob(i))*(dprob(i,m)/prob(i))
						end do
					end do		
				end do

				! update parameter values using BHHH formula	
				call update_parms(gradient, hessian, invhessian, freebeta, func, step, converged, feasible)
				! print iteration and function value
	           !
	           ! Small nudge to see if we can find an improving path
	           !
		 	   if(nudge .and. (nbnudge .lt. maxnudge)  ) then
		 	    if (converged .and. (best .ne. func)) then
	             best = max(best, func)
	             write(*,*) 'Randomly moving to validate estimation (best = )', best
	             write(*,*) 'Parameter values:', freebeta
	             converged = .false.
	             freebeta = (1 + (0.5-rand(0))/100 )*freebeta
	             nbnudge = nbnudge + 1
	            end if
	           end if



				if (.not. feasible) then
				    !!! Try moving randomly to find an improving path
					if (nudge .and. (optim .ne. func)) then
					  optim = func
					  write(*,*) 'iter = ', iter, 'func = ', func, 'step = ', step
					  write (*,*) '... failed line search. Moving to another location...'
					  freebeta = (1.0d0 + (0.5-rand(0))/100 )*freebeta
					  feasible = .true.

	                else
					  converged = .false.
				      write(*,*) ' ... failed line search, step = ',step
					  exit
					end if
				else
					write(*,*) 'iter = ', iter, 'func = ', func, 'step = ', step
				end if	
				! optional: write parameter, gradient and hessian values
				do i = 1, nfreepar, 1
				!	write(*,*) freebeta(i),gradient(i),hessian(i,i)
				end do
				iter = iter + 1

			end do

			! output for now
			write(*,*) 'results for scenario = ', adjustl(trim(scenario))
			if (converged) then
				write(*,*) 'converged,  loglikelihood =  ', func
			else
				write(*,*) 'no convergence, loglikelihood', func
			end if	
			do i = 1, nfreepar, 1
				write(*,'(I2,A1,A12,3F10.3)') i,' ',labfreepar(i),freebeta(i),sqrt(invhessian(i,i)), &
					freebeta(i)/sqrt(invhessian(i,i))
			end do
			
			! get whole parameter vector
			call getpar(freebeta, beta)
			
			! now save to file
			open(4, file='../params/parms_'//adjustl(trim(scenario))//'.csv')
			write(4,*) func
			write(4,*) npar
			ii = 1
			do i = 1, npar, 1
				if (fixpar(i) .eq. 999.0d0) then
					write(4,*) labpar(i),beta(i), sqrt(invhessian(ii,ii))
					ii = ii + 1
				else
					write(4,*) labpar(i),beta(i), 0.0d0
				end if	
			end do	
			close(4)
			
			!This allows to get the parameters back in the code
			freepar = freebeta
		end if	
	end subroutine doestimation
	
	! BHHH updating formula for parameters
	subroutine update_parms(gradient, hessian,invhessian, parms, func, lambda, converged, feasible)
		! inputs (gradient, hessian and free parameters)
		double precision gradient(nfreepar), hessian(nfreepar,nfreepar), parms(nfreepar)
		double precision func, newparms(nfreepar), probnew(n), oldparms(nfreepar)
		! converged is true if convergence achieved
		logical converged
		! intermediate arrays 
		double precision invhessian(nfreepar,nfreepar), work(nfreepar,nfreepar) 
		! Q*g where Q is inverse hessian and g is gradient
		double precision direction(nfreepar)					
		double precision condition, tolerance, lambda, step_up, step_down
		! function value at new direction
		double precision funcnew, funcold
		! integers
		integer iflag, j, t, count, i, info, ipiv(nfreepar)
		! logical
		logical improved, contract, expand, feasible
		! set tolerance for convergence
		tolerance = 1.0d-6
		lambda = 1.0d0
		step_up = 2.0d0
		step_down = 0.10d0
		funcold = func
		oldparms = parms

		! invert hessian to get Q, the positive definite matrix in the direction
		!call invert_nag(hessian, invhessian, iflag)
		! compute LU factorization using DGETRF
		invhessian = hessian
		call DGETRF( nfreepar, nfreepar, invhessian, nfreepar, ipiv, info )
		! now compute inverse
		call DGETRI( nfreepar, invhessian, nfreepar, ipiv, work, nfreepar, info )
		! update parameters
		! first evaluation of direction
		do j= 1, nfreepar, 1
			direction(j) = 0.0d0
			do t= 1,nfreepar, 1
				invhessian(j,t) = invhessian(j,t)
				direction(j) = direction(j) + invhessian(j,t)*gradient(t)
			end do	
			newparms(j) = parms(j) + lambda*direction(j)		
		end do	
		
		improved = .false.
		count = 0

		! check if improvement at lambda = 1
		! compute new function using new lambda
		expand = .false.
		contract = .false.
		feasible = .true.

		call getprob(newparms,probnew)

		funcnew = 0.0d0
		do i = 1, n, 1
			funcnew = funcnew + dlog(probnew(i))
		end do
		! check if improvement observed, if not will need to contract

		if (funcnew .lt. funcold .or. isnan(funcnew)) then
			contract = .true.
			funcnew = funcold
			newparms = parms
		else
			expand = .true.
		end if	
	
		! now expand until there is no improvement
		if (expand) then
			do while (.not. improved)
				! save current function and parms
				funcold = funcnew
				oldparms = newparms				
				! step up lambda
					lambda = lambda * step_up
				! compute new parameters
				do j = 1, nfreepar, 1
					newparms(j) = parms(j) + lambda*direction(j)
				end do	
				! evaluate function
				call getprob(newparms,probnew)

				funcnew = 0.0d0
				do i = 1, n, 1
					funcnew = funcnew + dlog(probnew(i))
				end do
				! check whether there is improvement
				if (funcnew .ge. funcold) then
					continue
				else
					! get out
					improved = .true.
					newparms = oldparms
					funcnew = funcold
					exit
				end if	
			end do
		end if
		count = 0
		if (contract) then
			do while ((.not. improved) .and. (feasible .eqv. .true.))
				count = count + 1
				! save current function and parms
				funcold = func
				oldparms = parms
				! reduce lambda
					lambda = lambda * step_down
				! compute new parameters
				do j = 1, nfreepar, 1
					newparms(j) = parms(j) + lambda*direction(j)
				end do	
				! evaluate function
				call getprob(newparms,probnew)
				funcnew = 0.0d0
				do i = 1, n, 1
					funcnew = funcnew + dlog(probnew(i))
				end do
				! check whether there is improvement
				if (funcnew .ge. funcold) then
					improved = .true.
					exit
				else if (count .gt. 5) then
					improved = .false.
					feasible = .false.
					funcnew = func
					newparms = parms
					exit
				else 
					continue
				end if
			end do
		end if	
	
		!write(*,*) 'feasible = ', feasible, 'improved = ', improved
			
		! check for convergence using distance function
		if (feasible) then
			condition = 0.0d0
			do j=1,nfreepar
				condition = condition + (newparms(j)- parms(j))**2
			end do	
			if (condition.le.tolerance) then
				converged = .true.
			end if
			
			! update parameters and function
			func = funcnew
			parms = newparms
		else
			converged = .false.
		end if
		
		
	end subroutine update_parms
		
	subroutine postestimation
	! Used to show implications of results
        double precision:: beta(npar), rr
        integer :: i, j, nposm, nposf, aam, aaf, old, hh
	    integer m, f, c
	    integer agemin, agemax, nages, nper, agedif(n), ageretm, ageretf, njoint
	    logical mask(n), enough
        ! integers
        double precision bm(4,nxm), bf(4,nxf), bz(nz), bum(n), buf(n), buma, bufa, um, uf
        double precision rho(2),cutm(nm+1), cutf(nm+1), sigm, sigf, Lo(2,2), uscale(3)
        double precision VarCovType(2,2), lm, lf, v, vm, vf, cons
        double precision val1, val2, posterior(n,2), Lbase(2), surv(2), w, totcprob
        double precision expret(n,2), jointret(n), base, maxp, draw
        ! get all parameters from freebeta array and fixpar array
        call getpar(freepar, beta)
        ! map parameters
        call maptopar(beta, bm, bf, bz, rho, cutm, cutf, sigm, sigf, Lo, uscale)
        ! start loop over respondents
        bm(2,5) = 0.0d0
        bf(2,5) = 0.0d0
	    !Manipulating the choleski to get bac variance covariance
	    print *, ' '
	    print *, 'Variance/covariance of heterogeneity'
	    VarCovType = matmul(Lo, transpose(Lo))
	    do i = 1,2
	       write(*, '(3F10.3,3F10.3)') VarCovType(i,:)
	    end do
	    print *, ' '
	    print *, 'Correlation across leisure heterogeneity'
	    print *, VarCovType(2,1)/(sqrt(VarCovType(1,1))*sqrt(VarCovType(2,2)))
	    print *, ' '
	    
	    ! stats on unobserved heterogeneity term
	    print *, 'Fraction with positive marginal utility of leisure (at L(s)=Lmax, age 65)'			
			nposm = 0
			do i = 1, n
				bum(i) = bm(2,1)
				do j = 2, nxm, 1
					bum(i) = bum(i) + Xm(i,j)*bm(2,j)
				end do	
				if ((bum(i) + 3.0d0*bm(2,2) + bm(4,1)*dlog(Lmax)) .ge. 0.0d0) then
					nposm = nposm + 1
				end if	
			end do
	    print *, 'Male:', dble(nposm)/dble(n)
	    	nposf = 0
			do i = 1, n
				buf(i) = bf(2,1)
				do j = 2, nxf, 1
					buf(i) = buf(i) + Xf(i,j)*bf(2,j)
				end do	
				if ((buf(i) + 3.0d0*bf(2,2) + bf(4,1)*dlog(Lmax)) .ge. 0.0d0) then
					nposf = nposf + 1
				end if	
			end do
	    print *, 'Female:', dble(nposf)/dble(n)

	    ! posterior on unobserved heterogeneity
	    call getposterior(freepar,posterior)

	    ! posterior marginal utility of leisure
	    nposm = 0
	    nposf = 0
	    do i = 1, n, 1
	    	bum(i) = bum(i) + posterior(i,1)
	    	buf(i) = buf(i) + posterior(i,2)
	    	!write(*,*) posterior(i,:)
	    	if (bum(i) + 3.0d0*bm(2,2) + bm(4,1)*dlog(Lmax) .gt. 0.0d0) then
	    		nposm = nposm + 1
	    	end if
	    	if (buf(i) + 3.0d0*bf(2,2) + bf(4,1)*dlog(Lmax) .gt. 0.0d0) then
	    		nposf = nposf + 1
	    	end if
	    		
	    end do
	    print *, 'Fraction with positive marginal utility of leisure (with posterior, age 65)'			
	    print *, 'Male:', dble(nposm)/dble(n)
	    print *, 'Female:', dble(nposf)/dble(n)

	    ! do reshuffling
	    if (ishufwages) then
	    	do i = n, 1, -1
	    		call random_number(draw)
	    		j = 1+floor(i*draw)
	    		wages(i,1) = wages(j,1)
	    		hours(i,1) = hours(j,1)
	    	end do

	    	do i = n, 1, -1
	    		call random_number(draw)
	    		j = 1+floor(i*draw)
	    		wages(i,2) = wages(j,2)
	    		hours(i,2) = hours(j,2)
	    	end do
	    end if
	    ! do reshuffling
	    if (ishufheter) then
	    	do i = n, 1, -1
	    		call random_number(draw)
	    		j = 1+floor(i*draw)
	    		bum(i) = bum(j)
	    	end do

	    	do i = n, 1, -1
	    		call random_number(draw)
	    		j = 1+floor(i*draw)
	    		buf(i) = buf(j)
	    	end do
	    end if

	    ! Scenario with Social Security, get utility for each option
	    agemin = 50
	    agemax = 80
	    nages = agemax - agemin + 1
	    nper = Time - agemin + 1
	    ! select couples who will be part of this analysis and compute agedif
	    do i = 1, n, 1
	    	mask(i) = .false.
	    	agedif(i) = age(i,1) - age(i,2)
	    	if (age(i,1) .le. 62) then
	    		if (age(i,2) .le. 62) then
	    			if (abs(agedif(i)).lt.6) then
	    				mask(i) = .true.
	    			end if 
	    		endif
	    	endif
	    end do	
		
		allocate(Hm_sim(n,nages,nages,nper))
		allocate(Hf_sim(n,nages,nages,nper))
		allocate(Y_sim(n,nages,nages,nper))
		allocate(value(n,nages,nages))
		allocate(cprob(n,nages,nages))
	
	    ! construct expected utilities (joint)
	    do i = 1, n, 1    	
	    	if (mask(i)) then
				w = 0.0d0
				do j = 1, nz, 1
					w = w + Z(i,j)*bz(j)
				end do
				if (w .ge. 5.0d0) then
					w = 1.0d0
				else if (w .le. -5.0d0) then
					w = 0.0d0
				else 	
					w = dexp(w)/(1.0d0+dexp(w))
					!write(*,*) 'w = ', w
				end if	    		

		    	do m = 1, nages, 1
		    		ageretm = agemin + m - 1
		    		do f = 1, nages, 1
		    			ageretf = agemin + f - 1
			    		if (age(i,1) .le. ageretm .and. age(i,2) .le. ageretf) then
			    			c = (m-1)*nages + f
			    			! need figure out what age they are, convention: reaches 62
		    				if (age(i,1) .gt. age(i,2)) then
		    					old = 1
		    					aam = age(i,1)
		    					aaf = age(i,2)
		    				else 
		    					old = 2
		    					aam = age(i,1)
		    					aaf = age(i,2)
		    				end if	
		    				value(i,m,f) = 0.0d0
		    				Lbase(1) = L(1,i,aam)
		    				Lbase(2) = L(2,i,aaf)
			    			do j = 1, nper, 1
			    				if (aam .lt. Time .and. aaf .lt. Time) then
				    				! compute hours and income
				    				Y_sim(i,m,f,j) = 0.0d0
				    				if (aam .lt. ageretm) then
				    					Hm_sim(i,m,f,j) = hours(i,1)
				    					Y_sim(i,m,f,j) = Y_sim(i,m,f,j) + wages(i,1)*hours(i,1)
				    				else
				    					Hm_sim(i,m,f,j) = 0.0d0
				    					rr = reprate
				    					if (ageretm .lt. 66) then
				    						rr = rr*dexp(arf*dble(ageretm - 66))
				    					else
				    						rr = rr*dexp(drc*dble(ageretm - 66))
				    					end if	
				    					Y_sim(i,m,f,j) = Y_sim(i,m,f,j) + wages(i,1)*hours(i,1)*rr
				    				end if	
				    				if (aaf .lt. ageretf) then
				    					Hf_sim(i,m,f,j) = hours(i,2)
				    					Y_sim(i,m,f,j) = Y_sim(i,m,f,j) + wages(i,2)*hours(i,2)
				    				else
				    					Hf_sim(i,m,f,j) = 0.0d0
				    					rr = reprate
				    					if (ageretf .lt. 66) then
				    						rr = rr*dexp(arf*dble(ageretf - 66))

				    					else
				    						rr = rr*dexp(drc*dble(ageretf - 66))

				    					end if	
				    					Y_sim(i,m,f,j) = Y_sim(i,m,f,j) + wages(i,2)*hours(i,2)*rr
				    				end if	
				    			
				    				! evaluate utility of husband
				    				vm = 0.0d0
									surv(1) = L(1,i,aam)/Lbase(1)
									surv(2) = L(2,i,aaf)/Lbase(2)
				    				do hh = 1, 4, 1
									    ! add current age to marginal utility
									    if (aam .lt. agemax) then
									    	buma = bum(i) + bm(2,2)*dble(aam - 62)
									    else
									    	buma = bum(i) + bm(2,2)*dble(agemax - 62)
									    end if	
									    ! make change for health limitations
									    if (aam .ge. 62) then
									    	buma = buma + bm(2,3)*(sHL(hh,1) - Xm(i,3))
										end if	
										lm = Lmax - Hm_sim(i,m,f,j)
										lf = Lmax - Hf_sim(i,m,f,j)
										if (iblockcomp) then
											lf = Lmax - sum(Hf_sim(:,m,f,j))/dble(n)
										end if	
										cons = Y_sim(i,m,f,j)
										call getutility(cons, lm, lf, bm(1,1), buma, bm(3,1), bm(4,1), 0.0d0, v)
										vm = vm + pHL(i,hh)*(rho(1)**(aam-age(i,1)))*surv(1)*v
				    				end do
				    				! evaluate utility of wife
				    				vf = 0.0d0
				    				do hh = 1, 4, 1
									    ! add current age to marginal utility
									    if (aaf .lt. agemax) then
									    	bufa = buf(i) + bf(2,2)*dble(aaf - 62)
										else
											bufa = buf(i) + bf(2,2)*dble(agemax - 62)
										end if
										! make change for health limitations
										if (aaf .ge. 62) then
									    	bufa = bufa + bf(2,3)*(sHL(hh,2) - Xf(i,3))
										end if
										lm = Lmax - Hm_sim(i,m,f,j)
										if (iblockcomp) then
											lm = Lmax - sum(Hm_sim(:,m,f,j))/dble(n)
										end if	
										lf = Lmax - Hf_sim(i,m,f,j)
										cons = Y_sim(i,m,f,j)
										call getutility(cons, lf, lm, bf(1,1), bufa, bf(3,1), bf(4,1), 0.0d0, v)
										vf = vf + pHL(i,hh)*(rho(2)**(aaf-age(i,2)))*surv(2)*v
				    				end do			    				
				    				! discount, weight and add to cumulative sum
				    				value(i,m,f) = value(i,m,f) + (w*vm + (1.0d0 - w)*vf)
				    				!write(*,*) agedif(i),aam, aaf, ageretm, ageretf, vm, vf
				    				aam = aam + 1
				    				aaf = aaf + 1
			    				end if

			    			end do
		    			else
		    				value(i,m,f) = -999.0d0

		    			end if
		    		end do
		    	end do

		
		    	! find a base
		    	enough = .true.
		    	do m = 1, nages, 1
		    		do f = 1, nages, 1
		    			if (value(i,m,f) .ne. -999.0d0 .and. enough) then
		    				base  = value(i,m,f) 
		    				enough = .false.
		    			end if	
		    		end do 
		    	end do	
		    	
		    	do m = 1, nages, 1
		    		do f = 1, nages, 1
		    			if (value(i,m,f) .gt. -999.0d0) then
		    				value(i,m,f) = value(i,m,f) - base	
		    			end if
		    		end do
		    	end do
			    ! choice probabilities
			    totcprob = 0.0d0
			    do m = 1, nages, 1
			    	do f = 1, nages, 1
			    		if (value(i,m,f) .gt. -999.0d0) then
			    			totcprob = totcprob + dexp(value(i,m,f))
			    		end if
			    	end do
			    end do
			    do m = 1, nages, 1
			    	do f = 1, nages, 1
			    		if (value(i,m,f) .gt. -999.0d0) then
			    			cprob(i,m,f) = dexp(value(i,m,f))/totcprob
			    		else
			    			cprob(i,m,f) = 0.0d0
			    		end if	
			    	end do
			    end do



			    ! get most preferred retirement age of each spouse
			    expret(i,:) = 0.0d0
			    maxp = 0.0d0
			    do m = 1, nages, 1
			    	ageretm = agemin + m - 1
			    	do f = 1, nages, 1
			    		ageretf = agemin + f - 1
!			    		expret(i,1) = expret(i,1) + cprob(i,m,f)*ageretm
!			    		expret(i,2) = expret(i,2) + cprob(i,m,f)*ageretf
						if (age(i,1) .le. ageretm .and. age(i,2) .le. ageretf) then
				    		if (cprob(i,m,f) .gt. maxp) then
				    			expret(i,1) = dble(ageretm)
				    			expret(i,2) = dble(ageretf)
				    			maxp = cprob(i,m,f)
				    		end if	
			    		end if
			    	end do
			    end do

			    ! get measure of joint retirement 0 years apart from each other
			    jointret(i) = 0.0d0
			    if ((expret(i,1) - expret(i,2)) .eq. agedif(i)) then
			    	jointret(i) = 1.0d0
			    end if	
			    !if ((expret(i,1) - expret(i,2)) .eq. agedif(i)-1.0d0) then
			    !	jointret(i) = 1.0d0
			    !end if	
			    !if ((expret(i,1) - expret(i,2)) .eq. agedif(i)+1.0d0) then
			    !	jointret(i) = 1.0d0
			    !end if	

			    !do m = 1, nages, 1
			    !	ageretm = agemin + m - 1
			    !	do f = 1, nages, 1
			    !		ageretf = agemin + f - 1
			    !		if ((ageretm - ageretf) .eq. agedif(i)) then
			    !			jointret(i) = jointret(i) + cprob(i,m,f)
			    !		end if	
			    !	end do
			    !end do
			 
			   ! write(*,*) 'probability of joint retirement: ', jointret(i)
	    	end if
	    end do

		open(2,file='../data/outcomes_'//adjustl(trim(scenario))//'.dat')
			do i = 1, n, 1
				if (mask(i)) then
					!write(*,*) age(i,1),age(i,2), id(i)
					write(2,*) id(i),1,jointret(i),expret(i,1),expret(i,2),posterior(i,:)
				else
					write(2,*) id(i),0,-999,-999,-999,posterior(i,:)
				end if
			end do   
   		close(2)
	   

	end subroutine postestimation

	subroutine getposterior(freebeta, posterior)
		double precision freebeta(nfreepar), beta(npar), prob(n), posterior(n,2)
		! integers
		integer i,u,j, s, k, agem(3), agef(3),hh
		! intermediate arrays double
		double precision fm(3), ff(3), w, lf,lm,cons,v,vm,vf,um,uf,vm1,vf1,vm2,vf2,vh1,vh2
		double precision  pf, ph, pu, pi, pm, pim, pif
		! arrays for parameters
		double precision bm(4,nxm), bf(4,nxf), bz(nz), bum, buf, buma, bufa
		double precision rho(2),cutm(nm+1), cutf(nm+1), sigm, sigf, Lo(2,2), uscale(3)
	

		! get choice probabilities
		call getprob(freebeta, prob)
		! get all parameters from freebeta array and fixpar array
		call getpar(freebeta, beta)
		! map parameters
		call maptopar(beta, bm, bf, bz, rho, cutm, cutf, sigm, sigf, Lo, uscale)
		! start loop over respondents

		do i = 1, n, 1	
			! compute discount factors (same across questions)
			call factor(i,age(i,1), age(i,2) , who(i), rho, fm, ff, agem, agef)
		
			! compute weight (same across questions)
			! initialize weight
			w = 0.0d0
			do j = 1, nz, 1
				w = w + Z(i,j)*bz(j)
			end do
			if (w .ge. 5.0d0) then
				w = 1.0d0
			else if (w .le. -5.0d0) then
				w = 0.0d0
			else 	
				w = dexp(w)/(1.0d0+dexp(w))
			end if
	
			
			! reduce marginal utility of leisure into bum and buf
			bum = bm(2,1)
			do j = 2, nxm, 1
				bum = bum + Xm(i,j)*bm(2,j)
			end do	
			buf = bf(2,1)
			do j = 2, nxf, 1
				buf = buf + Xf(i,j)*bf(2,j)
			end do	
			
			! initialize posterior
			pim = 0.0d0
			pif = 0.0d0
			! start looping over draws for UH
			do u = 1, nd, 1
				! update draws for UH
				um = Lo(1,1)*D(i,u,1) 
				uf = Lo(2,1)*D(i,u,1) + Lo(2,2)*D(i,u,2)
				! initialize probability p(i,u)
				pu = 1.0d0
				! start looping over questions, first nr are ratings, remaining choices
				do j = 1, nj, 1
			
					if (j.le.nr) then			! RATINGS QUESTIONS
						if (includeratings) then
							! there are two responses for each rating, husband and wife
							
							! HUSBAND
							! initialize husband's expected utility
							vm = 0.0d0
							! loop over three time intervals in scenario
							do s = 1, 3, 1
							    ! add current age to marginal utility
	                            buma = bum + bm(2,2)*(dble(agem(s)-62)- Xm(i,2))
								! compute log own leisure
								lm = Lmax - Hm(i,j,1,s)
								! compute other spouse leisure (normalized between 0 and 1)
								lf = Lmax - Hf(i,j,1,s)
								! compute log consumption
								cons = Y(i,j,1,s)
								! compute utility for this period
							    call getutility(cons, lm, lf, bm(1,1), buma, bm(3,1), bm(4,1), um, v)
								! discount and add to expected utility
								vm = vm + fm(s)*v
							end do
							! get probability of rating husband
							if (R(i,j,1) .lt. 99) then
								call getp_order(R(i,j,1), vm, cutm, nm, sigm, pm)
							else 
								pm = 1.0d0
							end if
							! WIFE
							! initialize wife's expected utility
							vf = 0.0d0
							! loop over three time intervals in scenario
							do s = 1, 3, 1
							    ! add current age to marginal utility
							    bufa = buf + bf(2,2)*(dble(agef(s)-62)-Xf(i,2) ) 
								! compute log own leisure
								lf = Lmax - Hf(i,j,1,s)
								! compute other spouse leisure (normalized between 0 and 1)
								lm = Lmax - Hm(i,j,1,s)
								! compute log consumption
								cons = Y(i,j,1,s)
								! compute utility for this period
								call getutility(cons, lf, lm, bf(1,1), bufa, bf(3,1), bf(4,1), uf, v)
								
								! discount and add
								vf = vf + ff(s)*v
							end do
							! get probability of rating wife
							if (R(i,j,2) .lt. 99) then
								call getp_order(R(i,j,2), vf, cutf, nm, sigf, pf)
							else
								pf = 1.0d0
							end if	
							! compute product over j for given draw u and individual i
							pu = pu*pm*pf
						end if
					else  							! CHOICE QUESTIONS
						
						! index of choice question
						k = j - nr
						! there are three responses per choice question (husband, wife, household)
						! for each question, there are two scenarios (options)
						! use following to change nc if want only subset of choice questions
						if (k.le.nc) then

						vm1 = 0.0d0
						vm2 = 0.0d0
						vf1 = 0.0d0
						vf2 = 0.0d0

						do hh = 1, 4, 1

							! HUSBAND
							! expected utility option 1
							
							do s = 1, 3, 1
							    ! add current age to marginal utility
							    buma = bum + bm(2,2)*(dble(agem(s)-62)-Xm(i,2))
							    ! make change for health limitations
							    buma = buma + bm(2,3)*(sHL(hh,1) - Xm(i,3))
								lm = Lmax - Hm(i,j,1,s)
								lf = Lmax - Hf(i,j,1,s)
								cons = Y(i,j,1,s)
								call getutility(cons, lm, lf, bm(1,1), buma, bm(3,1), bm(4,1), um, v)
								vm1 = vm1 + pHL(i,hh)*fm(s)*v
							end do
							! expected utility option 2
							
							do s = 1, 3, 1
							     ! add current age to marginal utility
							    buma = bum + bm(2,2)*(dble(agem(s)-62)-Xm(i,2)) 
							    ! make change for health limitations
							    buma = buma + bm(2,3)*(sHL(hh,1) - Xm(i,3))							    
								lm = Lmax - Hm(i,j,2,s)
								lf = Lmax - Hf(i,j,2,s)
								cons = Y(i,j,2,s)
								call getutility(cons, lm, lf, bm(1,1), buma, bm(3,1), bm(4,1), um, v)
								vm2 = vm2 + pHL(i,hh)*fm(s)*v
							end do

							! WIFE
							! expected utility option 1
							
							do s = 1, 3, 1
							    ! add current age to marginal utility
							    bufa = buf + bf(2,2)*(dble(agef(s)-62)-Xf(i,2)) 
							    ! make change for health limitations
							    bufa = bufa + bf(2,3)*(sHL(hh,2) - Xf(i,3))							    
								lf = Lmax - Hf(i,j,1,s)
								lm = Lmax - Hm(i,j,1,s)
								cons = Y(i,j,1,s)
								call getutility(cons, lf, lm, bf(1,1), bufa, bf(3,1), bf(4,1), uf, v)
								vf1 = vf1 + pHL(i,hh)*ff(s)*v
							end do
							! expected utility option 2
							
							do s = 1, 3, 1
							    ! add current age to marginal utility
							    bufa = buf + bf(2,2)*(dble(agef(s)-62)-Xf(i,2)) 
							    ! make change for health limitations
							    bufa = bufa + bf(2,3)*(sHL(hh,2) - Xf(i,3))							    
								lf = Lmax - Hf(i,j,2,s)
								lm = Lmax - Hm(i,j,2,s)
								cons = Y(i,j,2,s)
								call getutility(cons, lf, lm, bf(1,1), bufa, bf(3,1), bf(4,1), uf, v)
								vf2 = vf2 + pHL(i,hh)*ff(s)*v
							end do		

						end do
						
						! HOUSEHOLD
						! expected utility household option 1
						vh1 = w*vm1 + (1.0d0-w)*vf1
						! expected utility household option 2
						vh2 = w*vm2 + (1.0d0-w)*vf2


						! probability observing choice
						if (C(i,k,1) .eq. 1) then
							pm =  probl((vm1 - vm2)/uscale(1))
						else if (C(i,k,1) .eq. 2) then
							pm = probl((vm2 - vm1)/uscale(1))
						else 
						    pm = 1.0d0	
						end if

						! probability observing choice
						if (C(i,k,2) .eq. 1) then
							pf = probl((vf1-vf2)/uscale(2))
						else if (C(i,k,2) .eq. 2) then
							pf = probl((vf2-vf1)/uscale(2))
						else
						    pf = 1.0d0	
						end if
	
						! probability observing choice option 
						if (C(i,k,3).eq.1) then
							ph = probl((vh1-vh2)/uscale(3))
						else if (C(i,k,3) .eq. 2) then
							ph = probl((vh2-vh1)/uscale(3))
						else 
						    ph = 1.0d0	
						end if
						
						! product over j for given u						
						pu = pu*pm*pf*ph	
						end if
					end if ! end if statement over question type

				end do ! end loop questions over questions
			! sum over draws for respondent i
			pim = pim + pu/prob(i)*um
			pif = pif + pu/prob(i)*uf


			end do ! end loop over draws	
			! probability of respondent i (average over p(i,u))
			posterior(i,1) = pim/dble(nd)
			posterior(i,2) = pif/dble(nd)

			!write(*,*) pro


		end do ! end loop respondents	
		
	end subroutine getposterior

	double precision function probn(value)
		double precision value,  q, bound
		integer ifail, status
		if (value.ge.6.0d0) then
			value = 6.0d0
		else if (value.le. -6.0d0) then
			value = -6.0d0
		end if	
		call cdfnor(1,probn,q, value, 0.0d0, 1.0d0, status, bound)
	end function

	double precision function probl(value)
		double precision value,  q, bound
		integer ifail, status
		if (value.ge.6.0d0) then
			value = 6.0d0
		else if (value.le. -6.0d0) then
			value = -6.0d0
		end if	
		probl = dexp(value)/(1.0d0 + dexp(value))

	end function

	double precision function quann(prob)
		double precision prob, bound
		integer ifail, status
		call cdfnor(2,prob,1.0d0-prob, quann, 0.0d0, 1.0d0, status, bound)
	end function

end module sp 
