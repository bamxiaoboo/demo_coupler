module coupling_atm_model_mod

    use CCPL_interface_mod
    
    implicit none
    
    integer, private, parameter           :: RKIND = 4
    integer, private                      :: decomp_id, grid_h2d_id
    integer, public                       :: licom_comp_id
    integer, allocatable, public          :: mask_land(:)
    
    
contains

    subroutine register_licom_component(comm)
        integer, intent(inout) :: comm
        licom_comp_id = CCPL_register_component(-1, "licom", "ocn", comm, change_dir=.True., annotation = "register atm model licom")
    end subroutine register_licom_component
    
end module coupling_atm_model_mod
