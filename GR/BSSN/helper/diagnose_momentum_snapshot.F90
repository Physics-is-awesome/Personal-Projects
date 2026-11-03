! Re-evaluate the production momentum constraint on a saved BSSN snapshot.
! Usage: diagnose_momentum_snapshot <parfile> <state.h5> [initial.h5] [target_z]
program diagnose_momentum_snapshot
  use kinds_mod, only: dp
  use params_mod, only: Nx, Nz, dx, dz, r_excise, bh_spin, init_params
  use grid_mod, only: xg, zg, setup_grid
  use vars_mod
  use excision_mod, only: set_excision_background, excision_reset_geometry, &
                          extrapolate_excised, excision_status, PHYSICAL
  use deriv_stencils_mod, only: init_deriv_stencils
  use constraints_mod, only: momentum_constraint
  use hdf5
  implicit none

  character(len=512) :: par_file,snapshot,background_file,arg
  character(len=8), parameter :: field_name(NVARS) = [character(len=8) :: &
       'phi', 'gxx', 'gxy', 'gxz', 'gyy', 'gyz', 'gzz', 'K', &
       'Axx', 'Axy', 'Axz', 'Ayy', 'Ayz', 'Azz', &
       'Gamx', 'Gamy', 'Gamz', 'alpha', 'betax', 'betay', 'betaz', 'Bx', 'By', 'Bz']
  real(dp), allocatable :: u(:,:,:),ubg(:,:,:),M(:,:,:),Mterms(:,:,:,:),Mdir(:,:,:,:)
  real(dp) :: redges(10),bandmax(9),bandsum2(9),ctmax(5)
  integer :: bandcount(9),ctcount(5),band_i(9),band_k(9),ib,probe_bins(4)
  integer :: i,k,ii,kk,ios,ic,kc,max_i,max_k,ia,ka
  real(dp) :: target_z,r_target,best_dr,r_here,dr,mnorm,max_m
  real(dp) :: alpha_min,alpha_max,phi_min,phi_max

  call get_command_argument(1,par_file)
  call get_command_argument(2,snapshot)
  if (len_trim(par_file)==0 .or. len_trim(snapshot)==0) then
    write(*,'(A)') 'usage: diagnose_momentum_snapshot <parfile> <state.h5> [initial.h5] [target_z]'
    error stop 2
  end if
  background_file='';call get_command_argument(3,background_file)
  if (len_trim(background_file)==0) background_file=snapshot
  target_z=-1.535248_dp
  arg=''; call get_command_argument(4,arg)
  if (len_trim(arg)>0) read(arg,*,iostat=ios) target_z

  call init_params(trim(par_file))
  call setup_grid()
  allocate(u(Nx,Nz,NVARS),ubg(Nx,Nz,NVARS),M(Nx,Nz,3),Mterms(Nx,Nz,3,7), &
           Mdir(Nx,Nz,3,3))
  call read_snapshot(trim(snapshot),u)
  if (trim(background_file)==trim(snapshot)) then
    ubg=u
  else
    call read_snapshot(trim(background_file),ubg)
  end if

  call excision_reset_geometry()
  call set_excision_background(ubg)
  call extrapolate_excised(u)
  call init_deriv_stencils()
  call momentum_constraint(u,M,Mterms,Mdir)

  redges=[r_excise,r_excise+0.05_dp,r_excise+0.10_dp,r_excise+0.30_dp, &
          r_excise+0.50_dp,r_excise+1.50_dp,r_excise+3.50_dp, &
          r_excise+6.50_dp,r_excise+10.50_dp,huge(1.0_dp)]
  bandmax=0.0_dp;bandsum2=0.0_dp;bandcount=0;band_i=0;band_k=0
  ctmax=0.0_dp;ctcount=0
  ia=min(2,Nx-1); ka=minloc(abs(zg-target_z),dim=1)
  r_target=sqrt(xg(ia)**2+zg(ka)**2)
  max_m=0.0_dp;max_i=0;max_k=0
  alpha_min=huge(1.0_dp);alpha_max=-huge(1.0_dp)
  phi_min=huge(1.0_dp);phi_max=-huge(1.0_dp)
  do kk=1,Nz
    do ii=1,Nx
      if (excision_status(ii,kk)/=PHYSICAL) cycle
      alpha_min=min(alpha_min,u(ii,kk,IALPHA));alpha_max=max(alpha_max,u(ii,kk,IALPHA))
      phi_min=min(phi_min,u(ii,kk,IPHI));phi_max=max(phi_max,u(ii,kk,IPHI))
      mnorm=sqrt(sum(M(ii,kk,:)**2))
      r_here=kerr_radius(xg(ii),zg(kk))
      if (mnorm>max_m) then
        max_m=mnorm;max_i=ii;max_k=kk
      end if
      do ios=1,9
        if (r_here>=redges(ios) .and. r_here<redges(ios+1)) then
          if (mnorm>bandmax(ios)) then
            bandmax(ios)=mnorm;band_i(ios)=ii;band_k(ios)=kk
          end if
          bandsum2(ios)=bandsum2(ios)+mnorm*mnorm
          bandcount(ios)=bandcount(ios)+1
          exit
        end if
      end do
      if (r_here>=r_excise .and. r_here<r_excise+0.30_dp) then
        ios=min(5,int(abs(zg(kk))/max(r_here,1.0e-30_dp)*5.0_dp)+1)
        ctmax(ios)=max(ctmax(ios),mnorm)
        ctcount(ios)=ctcount(ios)+1
      end if
    end do
  end do
  write(*,'(A,A)') 'snapshot: ',trim(snapshot)
  write(*,'(A,A)') 'excision background: ',trim(background_file)
  write(*,'(A,I0,A,I0,A,ES13.5)') 'grid=',Nx,'x',Nz,' max|M| physical=',max_m
  write(*,'(A,2ES13.5)') 'physical alpha min/max=',alpha_min,alpha_max
  write(*,'(A,2ES13.5)') 'physical phi min/max=',phi_min,phi_max
  if (max_i>0) write(*,'(A,2ES15.7,A,ES12.4)') 'max location x,z=', &
       xg(max_i),zg(max_k),' r_KS=',kerr_radius(xg(max_i),zg(max_k))
  write(*,'(A)') 'max|M| and rms|M| by Kerr-radius band:'
  do i=1,9
    if (bandcount(i)>0 .and. i<9) then
      write(*,'(A,2F7.3,2ES13.5,I9)') ' ',redges(i),redges(i+1),bandmax(i), &
           sqrt(bandsum2(i)/real(bandcount(i),dp)),bandcount(i)
    else if (bandcount(i)>0) then
      write(*,'(A,F7.3,A,2ES13.5,I9)') ' ',redges(i),'+',bandmax(i), &
           sqrt(bandsum2(i)/real(bandcount(i),dp)),bandcount(i)
    end if
  end do
  write(*,'(A)') 'max|M| in r_excise..r_excise+0.30 by |cos(theta)| bin:'
  do i=1,5
    if (ctcount(i)>0) write(*,'(A,F5.2,A,F5.2,ES13.5,I9)') ' ',0.2_dp*real(i-1,dp), &
         '..',0.2_dp*real(i,dp),ctmax(i),ctcount(i)
  end do
  probe_bins=[1,6,8,9]
  do ib=1,size(probe_bins)
    i=probe_bins(ib)
    if (band_i(i)>0) then
      write(*,'(A,I0,A,ES12.4)') 'max terms for radial band ',i,' |M|=',bandmax(i)
      call report_point('radial-band max',band_i(i),band_k(i))
    end if
  end do
  write(*,'(A,2ES15.7,A,I0)') 'near-axis x,z=',xg(ia),zg(ka),' mask=',excision_status(ia,ka)
  call report_point('near-axis',ia,ka)

  ic=0;kc=0;best_dr=huge(1.0_dp)
  do k=2,Nz-1
    do i=2,Nx-1
      if (xg(i)<5.0_dp*max(dx,dz)) cycle
      if (excision_status(i,k)/=PHYSICAL) cycle
      r_here=sqrt(xg(i)**2+zg(k)**2);dr=abs(r_here-r_target)
      if (dr<best_dr) then
        best_dr=dr;ic=i;kc=k
      end if
    end do
  end do
  if (ic>0) then
    write(*,'(A,2ES15.7,A,ES12.4)') 'control x,z=',xg(ic),zg(kc),' |dr|=',best_dr
    call report_point('off-axis',ic,kc)
  end if

contains
  subroutine read_snapshot(path,state)
    character(len=*),intent(in)::path
    real(dp),intent(out)::state(Nx,Nz,NVARS)
    integer(HID_T)::fid,did
    integer(HSIZE_T)::shape(2)
    integer::hstat,v
    shape=[int(Nx,HSIZE_T),int(Nz,HSIZE_T)]
    call h5open_f(hstat)
    call h5fopen_f(trim(path),H5F_ACC_RDONLY_F,fid,hstat)
    if (hstat/=0) error stop 'could not open snapshot'
    do v=1,NVARS
      call h5dopen_f(fid,trim(field_name(v)),did,hstat)
      if (hstat/=0) error stop 'snapshot is missing a BSSN field'
      call h5dread_f(did,H5T_NATIVE_DOUBLE,state(:,:,v),shape,hstat)
      if (hstat/=0) error stop 'could not read snapshot field'
      call h5dclose_f(did,hstat)
    end do
    call h5fclose_f(fid,hstat)
    call h5close_f(hstat)
  end subroutine read_snapshot

  function kerr_radius(x,z) result(r)
    real(dp),intent(in)::x,z
    real(dp)::r,rho2,r2,a2
    a2=bh_spin*bh_spin;rho2=x*x+z*z
    r2=0.5_dp*((rho2-a2)+sqrt((rho2-a2)**2+4.0_dp*a2*z*z))
    r=sqrt(max(0.0_dp,r2))
  end function kerr_radius

  subroutine report_point(label,ii,kk)
    character(len=*),intent(in)::label
    integer,intent(in)::ii,kk
    integer::q
    mnorm=sqrt(sum(M(ii,kk,:)**2))
    write(*,'(A,A,A,3ES14.6,A,ES12.4)') '--- ',trim(label),' M=',M(ii,kk,:), &
         ' |M|=',mnorm
    do q=1,3
      write(*,'(A,I0,A,7(1X,ES13.5))') '  M',q,' pieces=',Mterms(ii,kk,q,:)
    end do
    write(*,'(A,3(1X,ES13.5))') '  dAup x-direction [Mx,My,Mz]=',Mdir(ii,kk,:,1)
    write(*,'(A,3(1X,ES13.5))') '  dAup y-direction [Mx,My,Mz]=',Mdir(ii,kk,:,2)
    write(*,'(A,3(1X,ES13.5))') '  dAup z-direction [Mx,My,Mz]=',Mdir(ii,kk,:,3)
  end subroutine report_point
end program diagnose_momentum_snapshot
