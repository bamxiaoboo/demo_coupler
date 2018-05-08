module coupling_ocn_model_mod

    use CCPL_interface_mod
    
    implicit none
    
    integer, private, parameter           :: RKIND = 4
    integer, private                      :: decomp_id, grid_h2d_id
    integer, public                       :: gamil_comp_id
    integer, allocatable, public          :: mask_land(:)
    real(kind=RKIND), allocatable, public :: sst(:), shf(:), ssh(:), mld(:)    
    
    
contains

    subroutine register_gamil_component(comm)
        integer, intent(inout) :: comm
        gamil_comp_id = CCPL_register_component(-1, "gamil", "atm", comm, change_dir=.True., annotation = "register atm model gamil")
    end subroutine register_gamil_component
    
    subroutine register_grids_decomps(nlat, nlon, lat, lon, decomp_size, local_id, npes, local_grid_cell_index)
        
        use CCPL_interface_mod
        
        implicit none
        integer, intent(in)          :: nlat, nlon
        integer, intent(in)          :: decomp_size, local_id, npes
        integer, intent(in)          :: local_grid_cell_index(decomp_size, npes)
        real(kind=RKIND), intent(in) :: lat(nlat), lon(nlon)
        
        !grid_h2d_id=CCPL_register_H2D_grid_via_global_data(gamil_comp_id, "gamil_H2D_grid", "LON_LAT", "degrees", "cyclic", nlon, nlat, 0, 360, -90, 90, lon, lat, annotation="register gamil H2D grid ")
        grid_h2d_id = CCPL_register_H2D_grid_via_file(gamil_comp_id, "gamil_H2D_grid","gamil.h0.0591-06.nc", annotation="register gamil H2D grid ")
        decomp_id = CCPL_register_normal_parallel_decomp("decomp_gamil_grid", grid_H2D_id, decomp_size, local_grid_cell_index(:,local_id+1), &
                    annotation="allocate for gamil grid")
    end subroutine register_grids_decomps
     
    subroutine register_component_coupling_configuration(decomp_size, psl, ts, fsds, flds, time_step, &
            comp_id, comp_name, import_interface_id, export_interface_id)
       
       use CCPL_interface_mod
       
       implicit none
       
       integer, intent(in)                :: decomp_size
       real(kind=RKIND), intent(in)       :: psl(decomp_size), ts(decomp_size), fsds(decomp_size), flds(decomp_size)
       character(len=*), intent(in)       :: comp_name
       integer, intent(inout)             :: comp_id
       integer, intent(out)               :: export_interface_id, import_interface_id
       
       character*1024                     :: annotation
       integer                            :: time_step, timer1_id, fields_id(5)
       integer                            :: field_id_psl, field_id_ts, field_id_flds, field_id_fsds
       integer                            :: field_id_sst, field_id_ssh, field_id_shf, field_id_mld

       allocate(sst(decomp_size))
       allocate(shf(decomp_size))
       allocate(ssh(decomp_size))
       allocate(mld(decomp_size))
       
       field_id_psl = CCPL_register_field_instance(psl(1:decomp_size), "psl", decomp_id, grid_h2d_id, 0, usage_tag=CCPL_TAG_CPL_REST, field_unit="Pa", annotation="register field instance of Sea level pressure") 
       field_id_ts = CCPL_register_field_instance(ts(1:decomp_size), "ts", decomp_id, grid_h2d_id, 0, usage_tag=CCPL_TAG_CPL_REST, field_unit="K", annotation="register field instance of Surface temperature")
       field_id_fsds = CCPL_register_field_instance(fsds(1:decomp_size), "fsds", decomp_id, grid_h2d_id, 0, usage_tag=CCPL_TAG_CPL_REST, field_unit="W/m2", annotation="register field instance of Short wave downward flux at surface")
       field_id_flds  = CCPL_register_field_instance(flds(1:decomp_size), "flds", decomp_id, grid_h2d_id, 0, usage_tag=CCPL_TAG_CPL_REST, field_unit="W/m2", annotation="register field instance of Long wave downward flux at surface")
       field_id_sst  = CCPL_register_field_instance(sst, "sst", decomp_id, grid_h2d_id, 0, usage_tag=CCPL_TAG_CPL_REST, field_unit="C", annotation="register field instance of Sea surface temperature")
       field_id_shf  = CCPL_register_field_instance(shf, "shf", decomp_id, grid_h2d_id, 0, usage_tag=CCPL_TAG_CPL_REST, field_unit="W/m2", annotation="register field instance of Net surface heat flux")
       field_id_ssh = CCPL_register_field_instance(ssh, "ssh", decomp_id, grid_h2d_id, 0, usage_tag=CCPL_TAG_CPL_REST, field_unit="m", annotation="register field instance of Sea surface height")
       field_id_mld = CCPL_register_field_instance(mld, "mld", decomp_id, grid_h2d_id, 0, usage_tag=CCPL_TAG_CPL_REST, field_unit="m", annotation="register field instance of Mixed layer depth")
       
       call CCPL_set_normal_time_step(comp_id, time_step, annotation="setting the time step for gamil")
       annotation = "component "//comp_name//" start registration"
       timer1_id = CCPL_define_single_timer(comp_id, "steps", 1, 0, 0, annotation="define a single timer for comp_id_gamil")
       
       !allocate(fields_id(5))
       fields_id(1) = field_id_psl
       fields_id(2) = field_id_ts
       fields_id(3) = field_id_fsds
       fields_id(4) = field_id_flds
       export_interface_id = CCPL_register_export_interface("send_data_to_ocn", 4, fields_id, timer1_id, annotation="register interface for sending data to ocean")
       
       fields_id(1) = field_id_sst
       fields_id(2) = field_id_shf
       fields_id(3) = field_id_ssh
       fields_id(4) = field_id_mld
       import_interface_id = CCPL_register_import_interface("receive_data_from_ocn", 4, fields_id, timer1_id, 0, annotation="register interface for receiving data from ocean")
       call CCPL_do_individual_coupling_generation(comp_id, annotation= "component "//comp_name//" generates the coupling procedure")
       call CCPL_end_coupling_configuration(comp_id, annotation = "component "//comp_name//" end registration")
       !deallocate(fields_id)

    end subroutine register_component_coupling_configuration    

end module coupling_ocn_model_mod
