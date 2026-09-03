PROGRAM WHCKL
  use init
  use kepler
  use init_val
  use symplectic_corrector
  use precision
  use interaction
  use transform
  use vtk_output

  IMPLICIT NONE
  type(nbody_state) :: state
  integer :: n = 15
  real(real64) :: dt, h

  integer(int64) :: wall_count_start, wall_count_now, count_rate
  real(real64) :: cpu_start, cpu_now
  real(real64) :: wall_seconds, cpu_seconds

  ! ----------------------------------------------------------
  ! Wall-clock time budget for this run. 23.5h leaves margin
  ! inside a 24h window for setup/shutdown/file flushing.
  ! Change this to whatever your actual budget is.
  ! ----------------------------------------------------------
  real(real64), parameter :: time_budget_seconds = 3.0_real64         ! 23.5_real64 * 3600.0_real64

  ! How often (in steps) to check the wall clock and write a
  ! performance-log row. Calibrated from a measured throughput of
  ! ~2.1e5 steps/sec for this 15-body system on the hardware this
  ! was developed on -- targets ~20,000 log rows over a 23.5h run
  ! (a reasonable size to load and graph). If your hardware's
  ! throughput differs meaningfully, rescale this proportionally
  ! (check_every = expected_total_steps / desired_row_count).
  integer, parameter :: check_every = 1

  ! Animation frame cadence -- targets ~1000 frames over the full
  ! run (same calibration basis as check_every above).
  integer, parameter :: frame_every = 1

  integer(int64) :: step_count
  integer :: perf_unit, manifest_unit, frame_number
  character(len=64) :: fname
  real(real64) :: E, E0

  call initialize(state, n)
  call initial_fundemental_values(state, dt)
  h = dt

  call cartesian_to_jacobi(state)
  call corrector_11(state, h)

  call jacobi_to_cartesian(state)
  E0 = energy(state)

  ! ----------------------------------------------------------
  ! Continuous performance log (CSV) -- one row every
  ! check_every steps: step count, simulated time, wall-clock
  ! elapsed, CPU elapsed, throughput, and energy conservation
  ! (dE/E0) so you can plot both performance AND long-term
  ! accuracy from the same file.
  ! ----------------------------------------------------------
  open(newunit=perf_unit, file='performance_log.csv', status='replace', &
       action='write', form='formatted')
  write(perf_unit,'(A)') 'step,sim_time_years,wall_seconds,cpu_seconds,steps_per_sec,dE_over_E0'

  open(newunit=manifest_unit, file='frames_manifest.csv', status='replace', &
       action='write', form='formatted')
  write(manifest_unit,'(A)') 'frame,sim_time_years,filename'

  frame_number = 0

  call system_clock(wall_count_start, count_rate)
  call cpu_time(cpu_start)

  step_count = 0_int64

  do

    call kepler_step(state, dt * 0.5_real64)
    call jacobi_interaction_kick(state, dt)
    call kepler_step(state, dt * 0.5_real64)

    step_count = step_count + 1_int64

    if (mod(step_count, int(check_every, int64)) == 0_int64) then

      call system_clock(wall_count_now)
      call cpu_time(cpu_now)
      wall_seconds = real(wall_count_now - wall_count_start, real64) / real(count_rate, real64)
      cpu_seconds  = cpu_now - cpu_start

      call jacobi_to_cartesian(state)
      E = energy(state)

      write(perf_unit,'(I0,A,F14.4,A,F14.4,A,F14.4,A,F14.4,A,ES20.6)') &
        step_count, ',', real(step_count,real64)*dt, ',', wall_seconds, ',', &
        cpu_seconds, ',', real(step_count,real64)/wall_seconds, ',', (E-E0)/E0
      flush(perf_unit)

      if (mod(step_count, int(frame_every, int64)) == 0_int64) then
        frame_number = frame_number + 1
        write(fname, '(A,I6.6,A)') 'solar_system_', frame_number, '.vtp'
        call write_vtk_frame(state, fname)
        write(manifest_unit,'(I0,A,F14.4,A,A)') &
          frame_number, ',', real(step_count,real64)*dt, ',', trim(fname)
        flush(manifest_unit)
      end if

      ! Stop once the wall-clock budget is used up.
      if (wall_seconds >= time_budget_seconds) exit

    end if

  end do

  call inverse_corrector_11(state, h)
  call jacobi_to_cartesian(state)

  ! final frame + final performance row
  frame_number = frame_number + 1
  write(fname, '(A,I6.6,A)') 'solar_system_', frame_number, '.vtp'
  call write_vtk_frame(state, fname)
  write(manifest_unit,'(I0,A,F14.4,A,A)') &
    frame_number, ',', real(step_count,real64)*dt, ',', trim(fname)
  close(manifest_unit)

  E = energy(state)
  write(perf_unit,'(I0,A,F14.4,A,F14.4,A,F14.4,A,F14.4,A,ES14.6)') &
    step_count, ',', real(step_count,real64)*dt, ',', wall_seconds, ',', &
    cpu_seconds, ',', real(step_count,real64)/wall_seconds, ',', (E-E0)/E0
  close(perf_unit)

  print '(A)', '=========================================='
  print '(A,I0)',      'Bodies                    : ', n
  print '(A,I0)',      'Total steps completed      : ', step_count
  print '(A,F16.4,A)', 'Simulated time             : ', real(step_count,real64)*dt, ' years'
  print '(A,F12.4,A)', 'Wall-clock time            : ', wall_seconds, ' s'
  print '(A,F12.4,A)', 'CPU time                   : ', cpu_seconds,  ' s'
  print '(A,F8.4)',    'CPU utilization (cpu/wall) : ', cpu_seconds/wall_seconds
  print '(A,ES12.4,A)','Throughput                 : ', real(step_count,real64)/wall_seconds, ' steps/s'
  print '(A,ES12.4)',  'Final dE/E0                : ', (E-E0)/E0
  print '(A,I0)',      'Animation frames written   : ', frame_number
  print '(A)', '=========================================='
  print '(A)', 'Performance data: performance_log.csv'
  print '(A)', 'Animation frames: solar_system_NNNNNN.vtp + frames_manifest.csv'
  print '(A)', 'Run: python3 make_pvd.py   to build solar_system.pvd, then open it in ParaView.'

contains

  function energy(st) result(E)
    type(nbody_state), intent(in) :: st
    real(real64) :: E, v2, r
    integer :: a, b
    E = 0.0_real64
    do a = 1, st%n
      v2 = sum((st%p(:,a)/st%mass(a))**2)
      E = E + 0.5_real64*st%mass(a)*v2
    end do
    do a = 1, st%n-1
      do b = a+1, st%n
        r = sqrt(sum((st%q(:,a)-st%q(:,b))**2))
        E = E - st%G*st%mass(a)*st%mass(b)/r
      end do
    end do
  end function energy

END PROGRAM WHCKL
