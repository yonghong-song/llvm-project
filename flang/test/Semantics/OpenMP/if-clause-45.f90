! RUN: %python %S/../test_errors.py %s %flang -fopenmp -fopenmp-version=45
! Check OpenMP 'if' clause validity for all directives that can have it with OpenMP 45

program main
  integer :: i

  ! ----------------------------------------------------------------------------
  ! DISTRIBUTE PARALLEL DO
  ! ----------------------------------------------------------------------------
  !$omp teams
  !$omp distribute parallel do if(.true.)
  do i = 1, 10
  end do
  !$omp end distribute parallel do

  !$omp distribute parallel do if(parallel: .true.)
  do i = 1, 10
  end do
  !$omp end distribute parallel do

  !ERROR: TARGET is not a constituent of the DISTRIBUTE PARALLEL DO directive
  !$omp distribute parallel do if(target: .true.)
  do i = 1, 10
  end do
  !$omp end distribute parallel do

  !ERROR: At most one IF clause can appear on the DISTRIBUTE PARALLEL DO directive
  !$omp distribute parallel do if(.true.) if(parallel: .false.)
  do i = 1, 10
  end do
  !$omp end distribute parallel do
  !$omp end teams

  ! ----------------------------------------------------------------------------
  ! DISTRIBUTE PARALLEL DO SIMD
  ! ----------------------------------------------------------------------------
  !$omp teams
  !$omp distribute parallel do simd if(.true.)
  do i = 1, 10
  end do
  !$omp end distribute parallel do simd

  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp distribute parallel do simd if(parallel: .true.) if(simd: .false.)
  do i = 1, 10
  end do
  !$omp end distribute parallel do simd

  !ERROR: TARGET is not a constituent of the DISTRIBUTE PARALLEL DO SIMD directive
  !$omp distribute parallel do simd if(target: .true.)
  do i = 1, 10
  end do
  !$omp end distribute parallel do simd
  !$omp end teams

  ! ----------------------------------------------------------------------------
  ! DISTRIBUTE SIMD
  ! ----------------------------------------------------------------------------
  !$omp teams
  !ERROR: IF clause is not allowed on directive DISTRIBUTE SIMD in OpenMP v4.5, try -fopenmp-version=50
  !$omp distribute simd if(.true.)
  do i = 1, 10
  end do
  !$omp end distribute simd

  !ERROR: IF clause is not allowed on directive DISTRIBUTE SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp distribute simd if(simd: .true.)
  do i = 1, 10
  end do
  !$omp end distribute simd

  !ERROR: IF clause is not allowed on directive DISTRIBUTE SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: TARGET is not a constituent of the DISTRIBUTE SIMD directive
  !$omp distribute simd if(target: .true.)
  do i = 1, 10
  end do
  !$omp end distribute simd

  !ERROR: IF clause is not allowed on directive DISTRIBUTE SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: IF clause is not allowed on directive DISTRIBUTE SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: At most one IF clause can appear on the DISTRIBUTE SIMD directive
  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp distribute simd if(.true.) if(simd: .false.)
  do i = 1, 10
  end do
  !$omp end distribute simd
  !$omp end teams

  ! ----------------------------------------------------------------------------
  ! DO SIMD
  ! ----------------------------------------------------------------------------
  !ERROR: IF clause is not allowed on directive DO SIMD in OpenMP v4.5, try -fopenmp-version=50
  !$omp do simd if(.true.)
  do i = 1, 10
  end do
  !$omp end do simd

  !ERROR: IF clause is not allowed on directive DO SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp do simd if(simd: .true.)
  do i = 1, 10
  end do
  !$omp end do simd

  !ERROR: IF clause is not allowed on directive DO SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: TARGET is not a constituent of the DO SIMD directive
  !$omp do simd if(target: .true.)
  do i = 1, 10
  end do
  !$omp end do simd

  !ERROR: IF clause is not allowed on directive DO SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: IF clause is not allowed on directive DO SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: At most one IF clause can appear on the DO SIMD directive
  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp do simd if(.true.) if(simd: .false.)
  do i = 1, 10
  end do
  !$omp end do simd

  ! ----------------------------------------------------------------------------
  ! PARALLEL
  ! ----------------------------------------------------------------------------
  !$omp parallel if(.true.)
  !$omp end parallel

  !$omp parallel if(parallel: .true.)
  !$omp end parallel

  !ERROR: TARGET is not a constituent of the PARALLEL directive
  !$omp parallel if(target: .true.)
  !$omp end parallel

  !ERROR: At most one IF clause can appear on the PARALLEL directive
  !$omp parallel if(.true.) if(parallel: .false.)
  !$omp end parallel

  ! ----------------------------------------------------------------------------
  ! PARALLEL DO
  ! ----------------------------------------------------------------------------
  !$omp parallel do if(.true.)
  do i = 1, 10
  end do
  !$omp end parallel do

  !$omp parallel do if(parallel: .true.)
  do i = 1, 10
  end do
  !$omp end parallel do

  !ERROR: TARGET is not a constituent of the PARALLEL DO directive
  !$omp parallel do if(target: .true.)
  do i = 1, 10
  end do
  !$omp end parallel do

  !ERROR: At most one IF clause can appear on the PARALLEL DO directive
  !$omp parallel do if(.true.) if(parallel: .false.)
  do i = 1, 10
  end do
  !$omp end parallel do

  ! ----------------------------------------------------------------------------
  ! PARALLEL DO SIMD
  ! ----------------------------------------------------------------------------
  !$omp parallel do simd if(.true.)
  do i = 1, 10
  end do
  !$omp end parallel do simd

  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp parallel do simd if(parallel: .true.) if(simd: .false.)
  do i = 1, 10
  end do
  !$omp end parallel do simd

  !ERROR: TARGET is not a constituent of the PARALLEL DO SIMD directive
  !$omp parallel do simd if(target: .true.)
  do i = 1, 10
  end do
  !$omp end parallel do simd

  ! ----------------------------------------------------------------------------
  ! PARALLEL SECTIONS
  ! ----------------------------------------------------------------------------
  !$omp parallel sections if(.true.)
  !$omp end parallel sections

  !$omp parallel sections if(parallel: .true.)
  !$omp end parallel sections

  !ERROR: TARGET is not a constituent of the PARALLEL SECTIONS directive
  !$omp parallel sections if(target: .true.)
  !$omp end parallel sections

  !ERROR: At most one IF clause can appear on the PARALLEL SECTIONS directive
  !$omp parallel sections if(.true.) if(parallel: .false.)
  !$omp end parallel sections

  ! ----------------------------------------------------------------------------
  ! PARALLEL WORKSHARE
  ! ----------------------------------------------------------------------------
  !$omp parallel workshare if(.true.)
  !$omp end parallel workshare

  !$omp parallel workshare if(parallel: .true.)
  !$omp end parallel workshare

  !ERROR: TARGET is not a constituent of the PARALLEL WORKSHARE directive
  !$omp parallel workshare if(target: .true.)
  !$omp end parallel workshare

  !ERROR: At most one IF clause can appear on the PARALLEL WORKSHARE directive
  !$omp parallel workshare if(.true.) if(parallel: .false.)
  !$omp end parallel workshare

  ! ----------------------------------------------------------------------------
  ! SIMD
  ! ----------------------------------------------------------------------------
  !ERROR: IF clause is not allowed on directive SIMD in OpenMP v4.5, try -fopenmp-version=50
  !$omp simd if(.true.)
  do i = 1, 10
  end do
  !$omp end simd

  !ERROR: IF clause is not allowed on directive SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp simd if(simd: .true.)
  do i = 1, 10
  end do
  !$omp end simd

  !ERROR: IF clause is not allowed on directive SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: TARGET is not a constituent of the SIMD directive
  !$omp simd if(target: .true.)
  do i = 1, 10
  end do
  !$omp end simd

  !ERROR: IF clause is not allowed on directive SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: IF clause is not allowed on directive SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: At most one IF clause can appear on the SIMD directive
  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp simd if(.true.) if(simd: .false.)
  do i = 1, 10
  end do
  !$omp end simd

  ! ----------------------------------------------------------------------------
  ! TARGET
  ! ----------------------------------------------------------------------------
  !$omp target if(.true.)
  !$omp end target

  !$omp target if(target: .true.)
  !$omp end target

  !ERROR: PARALLEL is not a constituent of the TARGET directive
  !$omp target if(parallel: .true.)
  !$omp end target

  !ERROR: At most one IF clause can appear on the TARGET directive
  !$omp target if(.true.) if(target: .false.)
  !$omp end target

  ! ----------------------------------------------------------------------------
  ! TARGET DATA
  ! ----------------------------------------------------------------------------
  !$omp target data map(tofrom: i) if(.true.)
  !$omp end target data

  !$omp target data map(tofrom: i) if(target data: .true.)
  !$omp end target data

  !ERROR: TARGET is not a constituent of the TARGET DATA directive
  !$omp target data map(tofrom: i) if(target: .true.)
  !$omp end target data

  !ERROR: At most one IF clause can appear on the TARGET DATA directive
  !$omp target data map(tofrom: i) if(.true.) if(target data: .false.)
  !$omp end target data

  ! ----------------------------------------------------------------------------
  ! TARGET ENTER DATA
  ! ----------------------------------------------------------------------------
  !$omp target enter data map(to: i) if(.true.)

  !$omp target enter data map(to: i) if(target enter data: .true.)

  !ERROR: TARGET is not a constituent of the TARGET ENTER DATA directive
  !$omp target enter data map(to: i) if(target: .true.)

  !ERROR: At most one IF clause can appear on the TARGET ENTER DATA directive
  !$omp target enter data map(to: i) if(.true.) if(target enter data: .false.)

  ! ----------------------------------------------------------------------------
  ! TARGET EXIT DATA
  ! ----------------------------------------------------------------------------
  !$omp target exit data map(from: i) if(.true.)

  !$omp target exit data map(from: i) if(target exit data: .true.)

  !ERROR: TARGET is not a constituent of the TARGET EXIT DATA directive
  !$omp target exit data map(from: i) if(target: .true.)
  
  !ERROR: At most one IF clause can appear on the TARGET EXIT DATA directive
  !$omp target exit data map(from: i) if(.true.) if(target exit data: .false.)

  ! ----------------------------------------------------------------------------
  ! TARGET PARALLEL
  ! ----------------------------------------------------------------------------
  !$omp target parallel if(.true.)
  !$omp end target parallel

  !$omp target parallel if(target: .true.) if(parallel: .false.)
  !$omp end target parallel

  !ERROR: SIMD is not a constituent of the TARGET PARALLEL directive
  !$omp target parallel if(simd: .true.)
  !$omp end target parallel

  ! ----------------------------------------------------------------------------
  ! TARGET PARALLEL DO
  ! ----------------------------------------------------------------------------
  !$omp target parallel do if(.true.)
  do i = 1, 10
  end do
  !$omp end target parallel do

  !$omp target parallel do if(target: .true.) if(parallel: .false.)
  do i = 1, 10
  end do
  !$omp end target parallel do

  !ERROR: SIMD is not a constituent of the TARGET PARALLEL DO directive
  !$omp target parallel do if(simd: .true.)
  do i = 1, 10
  end do
  !$omp end target parallel do

  ! ----------------------------------------------------------------------------
  ! TARGET PARALLEL DO SIMD
  ! ----------------------------------------------------------------------------
  !$omp target parallel do simd if(.true.)
  do i = 1, 10
  end do
  !$omp end target parallel do simd

  !$omp target parallel do simd if(target: .true.) if(parallel: .false.) &
  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp&                        if(simd: .true.)
  do i = 1, 10
  end do
  !$omp end target parallel do simd

  !ERROR: TEAMS is not a constituent of the TARGET PARALLEL DO SIMD directive
  !$omp target parallel do simd if(teams: .true.)
  do i = 1, 10
  end do
  !$omp end target parallel do simd

  ! ----------------------------------------------------------------------------
  ! TARGET SIMD
  ! ----------------------------------------------------------------------------
  !$omp target simd if(.true.)
  do i = 1, 10
  end do
  !$omp end target simd

  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp target simd if(target: .true.) if(simd: .false.)
  do i = 1, 10
  end do
  !$omp end target simd

  !ERROR: PARALLEL is not a constituent of the TARGET SIMD directive
  !$omp target simd if(parallel: .true.)
  do i = 1, 10
  end do
  !$omp end target simd

  ! ----------------------------------------------------------------------------
  ! TARGET TEAMS
  ! ----------------------------------------------------------------------------
  !$omp target teams if(.true.)
  !$omp end target teams

  !ERROR: TEAMS is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=52
  !$omp target teams if(target: .true.) if(teams: .false.)
  !$omp end target teams

  !ERROR: PARALLEL is not a constituent of the TARGET TEAMS directive
  !$omp target teams if(parallel: .true.)
  !$omp end target teams

  ! ----------------------------------------------------------------------------
  ! TARGET TEAMS DISTRIBUTE
  ! ----------------------------------------------------------------------------
  !$omp target teams distribute if(.true.)
  do i = 1, 10
  end do
  !$omp end target teams distribute

  !ERROR: TEAMS is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=52
  !$omp target teams distribute if(target: .true.) if(teams: .false.)
  do i = 1, 10
  end do
  !$omp end target teams distribute

  !ERROR: PARALLEL is not a constituent of the TARGET TEAMS DISTRIBUTE directive
  !$omp target teams distribute if(parallel: .true.)
  do i = 1, 10
  end do
  !$omp end target teams distribute

  ! ----------------------------------------------------------------------------
  ! TARGET TEAMS DISTRIBUTE PARALLEL DO
  ! ----------------------------------------------------------------------------
  !$omp target teams distribute parallel do if(.true.)
  do i = 1, 10
  end do
  !$omp end target teams distribute parallel do

  !$omp target teams distribute parallel do &
  !ERROR: TEAMS is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=52
  !$omp&   if(target: .true.) if(teams: .false.) if(parallel: .true.)
  do i = 1, 10
  end do
  !$omp end target teams distribute parallel do

  !ERROR: SIMD is not a constituent of the TARGET TEAMS DISTRIBUTE PARALLEL DO directive
  !$omp target teams distribute parallel do if(simd: .true.)
  do i = 1, 10
  end do
  !$omp end target teams distribute parallel do

  ! ----------------------------------------------------------------------------
  ! TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD
  ! ----------------------------------------------------------------------------
  !$omp target teams distribute parallel do simd if(.true.)
  do i = 1, 10
  end do
  !$omp end target teams distribute parallel do simd

  !$omp target teams distribute parallel do simd &
  !ERROR: TEAMS is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=52
  !$omp&   if(target: .true.) if(teams: .false.) if(parallel: .true.) &
  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp&   if(simd: .false.)
  do i = 1, 10
  end do
  !$omp end target teams distribute parallel do simd

  !ERROR: TASK is not a constituent of the TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD directive
  !$omp target teams distribute parallel do simd if(task: .true.)
  do i = 1, 10
  end do
  !$omp end target teams distribute parallel do simd

  ! ----------------------------------------------------------------------------
  ! TARGET TEAMS DISTRIBUTE SIMD
  ! ----------------------------------------------------------------------------
  !$omp target teams distribute simd if(.true.)
  do i = 1, 10
  end do
  !$omp end target teams distribute simd

  !$omp target teams distribute simd &
  !ERROR: TEAMS is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=52
  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp&   if(target: .true.) if(teams: .false.) if(simd: .true.)
  do i = 1, 10
  end do
  !$omp end target teams distribute simd

  !ERROR: PARALLEL is not a constituent of the TARGET TEAMS DISTRIBUTE SIMD directive
  !$omp target teams distribute simd if(parallel: .true.)
  do i = 1, 10
  end do
  !$omp end target teams distribute simd

  ! ----------------------------------------------------------------------------
  ! TARGET UPDATE
  ! ----------------------------------------------------------------------------
  !$omp target update to(i) if(.true.)
  
  !$omp target update to(i) if(target update: .true.)

  !ERROR: TARGET is not a constituent of the TARGET UPDATE directive
  !$omp target update to(i) if(target: .true.)

  !ERROR: At most one IF clause can appear on the TARGET UPDATE directive
  !$omp target update to(i) if(.true.) if(target update: .false.)

  ! ----------------------------------------------------------------------------
  ! TASK
  ! ----------------------------------------------------------------------------
  !$omp task if(.true.)
  !$omp end task

  !$omp task if(task: .true.)
  !$omp end task

  !ERROR: TARGET is not a constituent of the TASK directive
  !$omp task if(target: .true.)
  !$omp end task

  !ERROR: At most one IF clause can appear on the TASK directive
  !$omp task if(.true.) if(task: .false.)
  !$omp end task

  ! ----------------------------------------------------------------------------
  ! TASKLOOP
  ! ----------------------------------------------------------------------------
  !$omp taskloop if(.true.)
  do i = 1, 10
  end do
  !$omp end taskloop

  !$omp taskloop if(taskloop: .true.)
  do i = 1, 10
  end do
  !$omp end taskloop

  !ERROR: TARGET is not a constituent of the TASKLOOP directive
  !$omp taskloop if(target: .true.)
  do i = 1, 10
  end do
  !$omp end taskloop

  !ERROR: At most one IF clause can appear on the TASKLOOP directive
  !$omp taskloop if(.true.) if(taskloop: .false.)
  do i = 1, 10
  end do
  !$omp end taskloop

  ! ----------------------------------------------------------------------------
  ! TASKLOOP SIMD
  ! ----------------------------------------------------------------------------
  !$omp taskloop simd if(.true.)
  do i = 1, 10
  end do
  !$omp end taskloop simd

  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp taskloop simd if(taskloop: .true.) if(simd: .false.)
  do i = 1, 10
  end do
  !$omp end taskloop simd

  !ERROR: TARGET is not a constituent of the TASKLOOP SIMD directive
  !$omp taskloop simd if(target: .true.)
  do i = 1, 10
  end do
  !$omp end taskloop simd

  ! ----------------------------------------------------------------------------
  ! TEAMS
  ! ----------------------------------------------------------------------------
  !ERROR: IF clause is not allowed on directive TEAMS in OpenMP v4.5, try -fopenmp-version=52
  !$omp teams if(.true.)
  !$omp end teams

  !ERROR: IF clause is not allowed on directive TEAMS in OpenMP v4.5, try -fopenmp-version=52
  !ERROR: TEAMS is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=52
  !$omp teams if(teams: .true.)
  !$omp end teams

  !ERROR: IF clause is not allowed on directive TEAMS in OpenMP v4.5, try -fopenmp-version=52
  !ERROR: TARGET is not a constituent of the TEAMS directive
  !$omp teams if(target: .true.)
  !$omp end teams

  !ERROR: IF clause is not allowed on directive TEAMS in OpenMP v4.5, try -fopenmp-version=52
  !ERROR: IF clause is not allowed on directive TEAMS in OpenMP v4.5, try -fopenmp-version=52
  !ERROR: At most one IF clause can appear on the TEAMS directive
  !ERROR: TEAMS is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=52
  !$omp teams if(.true.) if(teams: .false.)
  !$omp end teams

  ! ----------------------------------------------------------------------------
  ! TEAMS DISTRIBUTE
  ! ----------------------------------------------------------------------------
  !$omp teams distribute if(.true.)
  do i = 1, 10
  end do
  !$omp end teams distribute

  !ERROR: TEAMS is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=52
  !$omp teams distribute if(teams: .true.)
  do i = 1, 10
  end do
  !$omp end teams distribute

  !ERROR: TARGET is not a constituent of the TEAMS DISTRIBUTE directive
  !$omp teams distribute if(target: .true.)
  do i = 1, 10
  end do
  !$omp end teams distribute

  !ERROR: At most one IF clause can appear on the TEAMS DISTRIBUTE directive
  !ERROR: TEAMS is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=52
  !$omp teams distribute if(.true.) if(teams: .true.)
  do i = 1, 10
  end do
  !$omp end teams distribute

  ! ----------------------------------------------------------------------------
  ! TEAMS DISTRIBUTE PARALLEL DO
  ! ----------------------------------------------------------------------------
  !$omp teams distribute parallel do if(.true.)
  do i = 1, 10
  end do
  !$omp end teams distribute parallel do

  !ERROR: TEAMS is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=52
  !$omp teams distribute parallel do if(teams: .true.) if(parallel: .false.)
  do i = 1, 10
  end do
  !$omp end teams distribute parallel do

  !ERROR: TARGET is not a constituent of the TEAMS DISTRIBUTE PARALLEL DO directive
  !$omp teams distribute parallel do if(target: .true.)
  do i = 1, 10
  end do
  !$omp end teams distribute parallel do

  ! ----------------------------------------------------------------------------
  ! TEAMS DISTRIBUTE PARALLEL DO SIMD
  ! ----------------------------------------------------------------------------
  !$omp teams distribute parallel do simd if(.true.)
  do i = 1, 10
  end do
  !$omp end teams distribute parallel do simd

  !$omp teams distribute parallel do simd &
  !ERROR: TEAMS is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=52
  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp&   if(teams: .true.) if(parallel: .true.) if(simd: .true.)
  do i = 1, 10
  end do
  !$omp end teams distribute parallel do simd

  !ERROR: TARGET is not a constituent of the TEAMS DISTRIBUTE PARALLEL DO SIMD directive
  !$omp teams distribute parallel do simd if(target: .true.)
  do i = 1, 10
  end do
  !$omp end teams distribute parallel do simd

  ! ----------------------------------------------------------------------------
  ! TEAMS DISTRIBUTE SIMD
  ! ----------------------------------------------------------------------------
  !ERROR: IF clause is not allowed on directive TEAMS DISTRIBUTE SIMD in OpenMP v4.5, try -fopenmp-version=50
  !$omp teams distribute simd if(.true.)
  do i = 1, 10
  end do
  !$omp end teams distribute simd

  !ERROR: IF clause is not allowed on directive TEAMS DISTRIBUTE SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: TEAMS is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=52
  !ERROR: IF clause is not allowed on directive TEAMS DISTRIBUTE SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: SIMD is not allowed as 'directive-name-modifier' in OpenMP v4.5, try -fopenmp-version=50
  !$omp teams distribute simd if(teams: .true.) if(simd: .true.)
  do i = 1, 10
  end do
  !$omp end teams distribute simd

  !ERROR: IF clause is not allowed on directive TEAMS DISTRIBUTE SIMD in OpenMP v4.5, try -fopenmp-version=50
  !ERROR: TARGET is not a constituent of the TEAMS DISTRIBUTE SIMD directive
  !$omp teams distribute simd if(target: .true.)
  do i = 1, 10
  end do
  !$omp end teams distribute simd
end program main
