module types
  implicit none
  
  ! type for mesh structures
  type :: mesh
    integer :: nx  ! grid size in x
    integer :: ny  ! grid size in y
    integer :: nz  ! grid size in z
    integer :: dim_run  ! diminsions in
    integer :: p  ! Polynomial, p+1=nodes per element
    real(8) :: dx  ! discritization value
  end type

  ! values for setting up initilization of main state variables
  type :: state_init
    character(len=256) :: mass_dist, temp_dist, momentum_dist, entropy_dist   ! distribution functions for state variables
    real(8) :: mass_mean, temp_mean, momentum_mean, entropy_mean  ! mean of state variables
    real(8) :: mass_var, temp_var, momentum_var, entropy_var  ! variance of state variabes
  end type

  ! type for main state variabes
  type :: State
    real(8), allocatable :: rho_h(:,:,:)  ! mass
    real(8), allocatable :: sigma_h(:,:,:)  ! entropy
    real(8), allocatable :: m_h(:,:,:)  ! momentum
  end type State

  ! constants used
  type :: constants
    real(8) :: Re  ! reynolds number
    real(8) :: gamma  ! isentropic exponent
    real(8) :: Pr
  end type

  ! derived variables
  type :: derived_vars
    real(8) :: U(:,:,:)   ! internal energt
    real(8) :: T_h(:,:,:)  ! temperature
    real(8) :: dT_h_dx(:,:,:)  ! derivative of temperature by space(just x)
    real(8) :: u_h(:,:,:)  ! velocity
    real(8) :: du_h_dx(:,:,:)  ! spacial derivative of velocity(just x)
    real(8) :: T_S(:,:,:)  ! ratio of spesific entropy to temperature
    real(8) :: pressure(:,:,:)  ! pressure 
    real(8) :: eta_h(:,:,:)  ! spesific entropy
    real(8) :: deta_h_dx(:,:,:)  ! special derivative of spesific entropy
  end type

  type :: Omega-X
    type(derived_vars) :: dv
    type(constants) :: const
    type(State) :: S
    type(state_init) :: inints
    type(mesh) :: m
  end type
