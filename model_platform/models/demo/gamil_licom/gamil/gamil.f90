module model_setting
    integer, parameter :: RKIND = 4
    integer, public :: npes, mpicom
    integer, public ::  latlen, lonlen
    real(kind=RKIND), public, allocatable :: PSL(:,:), TS(:,:), FLDS(:,:), FSDS(:,:)
    real(kind=RKIND), public, allocatable :: lat(:), lon(:)

    public arrays_are_the_same !check if two arrays with same dimensions are the same

    contains
    logical function arrays_are_the_same(global, global_back)
        implicit none
        real(kind=RKIND), intent(in) :: global(latlen, lonlen)
        real(kind=RKIND), intent(in) :: global_back(latlen, lonlen)
        integer :: i, j, m
    
        arrays_are_the_same = .true.
            do j = 1, latlen
                do m = 1, lonlen
                    if (global(j,m) .ne. global_back(j,m)) then
                        arrays_are_the_same = .false.
                    end if
                end do 
            end do
        return
    end function

    subroutine read_input_data(masterproc)
          
        implicit none
        include "netcdf.inc"
        
        logical, intent(in) :: masterproc
        character*1024 :: input_data_dir, input_file_name
        character*1024 :: input_file_dir_name
        integer :: ncid, ierr, varid, err
        integer :: decomp_size, local_grid_cell_index
        input_data_dir = "./"
        input_file_name = "gamil.h0.0591-06.nc"
        input_file_dir_name = input_data_dir//input_file_name
        latlen = 60
        lonlen = 128          
        if (masterproc) then
            allocate(lat(latlen), lon(lonlen))
            allocate(PSL(latlen, lonlen), TS(latlen, lonlen), FLDS(latlen, lonlen), FSDS(latlen, lonlen))
            
            ierr=NF_OPEN(trim(input_file_name),NF_NOWRITE,ncid)
            
            ierr=NF_INQ_VARID(ncid, 'lat', varid)
            ierr=NF_GET_VAR_REAL(ncid, varid, lat)
            ierr=NF_INQ_VARID(ncid, 'lon', varid)
            ierr=NF_GET_VAR_REAL(ncid, varid, lon)
            ierr=NF_INQ_VARID(ncid, 'PSL', varid)
            ierr=NF_GET_VAR_REAL(ncid, varid, PSL)
            ierr=NF_INQ_VARID(ncid, 'TS', varid)
            ierr=NF_GET_VAR_REAL(ncid, varid, TS)
            ierr=NF_INQ_VARID(ncid, 'FLDS', varid)
            ierr=NF_GET_VAR_REAL(ncid, varid, FLDS)
            ierr=NF_INQ_VARID(ncid, 'FSDS', varid)
            ierr=NF_GET_VAR_REAL(ncid, varid, FSDS)
            err=NF_CLOSE(ncid)
            !print*, TS(1:10,1)
            !print*, FLDS(1:10,1)
        else
            allocate(PSL(1,1), TS(1,1), FLDS(1,1), FSDS(1,1))
            allocate(lat(1), lon(1))              
        end if 

    end subroutine read_input_data

    subroutine scatter_field(global_field, local_field, local_grid_cell_indexes, decomp_size, masterproc, ier)
        use mpi
        implicit none
        
        integer, intent(in) :: decomp_size, ier
        integer, intent(in) :: local_grid_cell_indexes(decomp_size,npes)
        logical, intent(in) :: masterproc
        real(kind=RKIND), intent(in) :: global_field(latlen, lonlen)
        real(kind=RKIND), intent(out) :: local_field(decomp_size)
     
        !------------local variables---------------------------
        real(kind=RKIND) :: gfield(latlen*lonlen)
        real(kind=RKIND) :: lfield(decomp_size)
        
        integer :: p, i, j, m
        integer :: displs(1:npes) !scatter displacements
        integer :: sndcnts(1:npes) !scatter send counts
        integer :: recvcnt !scatter receive count
    
        logical :: check
        !number of grid points scattered to eache process
        
        sndcnts(:) = decomp_size
        displs(1) = 0
        do p=2, npes
            displs(p) = displs(p-1)+decomp_size
        end do
        recvcnt = decomp_size
    
        !copy field into global data structure
        if (masterproc) then
            j = 1
            do p=1,npes
                do i=1,decomp_size
                    m = ceiling((local_grid_cell_indexes(i,p)-0.5)/(latlen/1.0))
                    gfield(j) = global_field(local_grid_cell_indexes(i,p)-latlen*(m-1),m)
                    j = j+1
                end do
            end do
        end if
    
        !scatter to other processes
        call mpi_scatterv(gfield, sndcnts, displs, mpi_real4, &
            lfield, recvcnt, mpi_real4, 0, mpicom, ier)
        !copy into local data structure
        do i=1,decomp_size
            local_field(i) = lfield(i)
        end do
    end subroutine scatter_field

end module

program gamil
    use model_setting
    use mpi
    use coupling_ocn_model_mod

    implicit none
    
    integer :: ier, mpitask_id
    logical :: masterproc
    integer :: i, j
    real(kind=RKIND), allocatable :: PSL_l(:), TS_l(:)
    real(kind=RKIND), allocatable :: FLDS_l(:), FSDS_l(:)
    real(kind=RKIND), allocatable :: pslm(:),tsm(:)
    real(kind=RKIND), allocatable :: fldsm(:),fsdsm(:)
    integer :: time_step, time_length
    integer :: decomp_size
    integer, allocatable :: local_grid_cell_index(:,:)
    mpicom = mpi_comm_world
    call mpi_init(ier)
    call mpi_comm_rank(mpi_comm_world, mpitask_id, ier)
    call mpi_comm_size(mpicom, npes, ier)
    if (mpitask_id == 0) then
        masterproc = .true.
    else
        masterproc = .false.
    end if
    call read_input_data(masterproc)
    call mpi_bcast(latlen, 1, mpi_integer, 0, mpicom, ier)
    call mpi_bcast(lonlen, 1, mpi_integer, 0, mpicom, ier)
    !---setting up decomposition for licom----------------------
    decomp_size = latlen*lonlen/npes
    allocate(FLDS_l(decomp_size),TS_l(decomp_size))
    allocate(PSL_l(decomp_size),FSDS_l(decomp_size))
    if ((latlen*lonlen-decomp_size*npes) .ne. 0) then
        print *, "ERROR : grid cells cannot be equally decomposed to number of porcs"
    end if
    allocate(local_grid_cell_index(decomp_size,npes))
    do j = 1, npes
    do i = 1, decomp_size
      !local_grid_cell_index(i,j) = i+(j-1)*decomp_size
      local_grid_cell_index(i,j) = j+(i-1)*npes
    end do
    end do
    
    call scatter_field(TS, TS_l, local_grid_cell_index, decomp_size, masterproc, ier)
    call scatter_field(PSL, PSL_l, local_grid_cell_index, decomp_size, masterproc, ier)
    call scatter_field(FLDS, FLDS_l, local_grid_cell_index, decomp_size, masterproc, ier)
    call scatter_field(FSDS, FSDS_l, local_grid_cell_index, decomp_size, masterproc, ier)
    !print*, FLDS_l(1:10)
    !------------Entering time loop-----------------------------
    time_length = 3*3600     !in seconds
    time_step = 1800         !in seconds
    !assign variables for model processing
    allocate(pslm(decomp_size),tsm(decomp_size))
    allocate(fldsm(decomp_size),fsdsm(decomp_size))
    do i=1,time_length/time_step
          pslm = PSL_l
          tsm = TS_l
          fldsm = FLDS_l
          fsdsm = FSDS_l
    end do
    deallocate(pslm,tsm,fldsm,fsdsm)
    deallocate(FLDS_l,TS_l,PSL_l,FSDS_l)
    deallocate(local_grid_cell_index)
    deallocate(PSL, TS, FLDS, FSDS, lat, lon)
    call mpi_finalize(ier)
    print*,"gamil running completed"

end program
