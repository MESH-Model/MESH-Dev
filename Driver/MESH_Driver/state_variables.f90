!>
!> Description:
!>  Contains variable types for states of variables in the model, such
!>  as components of the water and energy balances, streamflow channels,
!>  and reservoirs.
!>
!> Instances of these types are accessible by the
!> 'sa_mesh_shared_variables' module: stas%
!>
module state_variables

    !> Type: flow_state
    !>  State of fluxes for flow.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  qi: Flow in to the element. [m3 s-1].
    !*  qo: Flow from the element. [m3 s-1].
    !*  s: Channel storage held in the element. [m3].
    type flow_state
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: qi, qo, s
    end type

    !> Type: river_flow (extends: flow_state)
    !>  State of fluxes for a river channel.
    !>
    !> Variables:
    type, extends(flow_state) :: river_flow

    end type

    !> Type: lake_flow (extends: flow_state)
    !>  State of fluxes for a water body.
    !>
    !> Variables:
    !*  ab: Volume abstracted at the end of the time-step. [m3].
    type, extends(flow_state) :: lake_flow
        real(kind = 4), dimension(:), allocatable :: ab
    end type

    !> Type: canopy
    !>  States of canopy.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  qac: Specific humidity of air within vegetation canopy space. [kg kg-1].
    !*  rcan: Intercepted liquid water stored on canopy. [kg m-2].
    !*  sncan: Intercepted frozen water stored on canopy. [kg m-2].
    !*  tac: Temperature of air within vegetation canopy. [K].
    !*  tcan: Vegetation canopy temperature. [K].
    !*  cmai: Aggregated mass of vegetation canopy. [kg m-2].
    !*  gro: Vegetation growth index.
    type canopy
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: qac, rcan, sncan, tac, tcan, cmai, gro
    end type

    !> Type: snow_balance
    !>  State of snow at the surface.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  sno: Mass of snow pack. [kg m-2].
    !*  albs: Snow albedo.
    !*  rhos: Density of snow. [kg m-3].
    !*  tsno: Snowpack temperature. [K].
    !*  wsno: Liquid water content of snow pack. [kg m-2].
    type snow_balance
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: sno, albs, rhos, tsno, wsno
    end type

    !> Type: surface_layer
    !>  States at the interface between the atmosphere and soil profile.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  tsfs: Ground surface temperature over subarea. [K].
    !*  tpnd: Temperature of ponded water. [K].
    !*  zpnd: Depth of ponded water on surface. [m].
    type surface_layer
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: tpnd, zpnd
        real(kind = 4), dimension(:, :), allocatable :: tsfs
    end type

    !> Type: soil_layer
    !>  States of the soil profile.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  thic: Volumetric frozen water content of soil layers. [m3 m-3].
    !*  thlq: Volumetric liquid water content of soil layers. [m3 m-3].
    !*  tbar: Temperature of soil layers. [K].
    !*  tbas: Temperature of bedrock in third soil layer. [K].
    type soil_layer
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: tbas
        real(kind = 4), dimension(:, :), allocatable :: thic, thlq, tbar
    end type

    !> Type: deep_zone
    !>  States of deep zone storage.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  zlw: Depth of water. [m].
    !*  tbas: Temperature. [m].
    type deep_zone
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: zlw, tbas
    end type

    !> Type: storage_state
    !>  Storage state at the beginning and end of the time-step.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  s1: Storage at the beginning of the time-step.
    !*  s2: Storage at the end of the time-step.
    type storage_state
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: s, ds
    end type

    !> Type: water_storage (extends: storage_state)
    !>  States of water held by different parts of the vertical profile.
    !>
    !> Inherited:
    !*  s1: Storage at the beginning of the time-step. [kg m-2].
    !*  s2: Storage at the end of the time-step. [kg m-2].
    !>
    !> Variables:
    !*  cnpy: Water held in the canopy. [kg m-2].
    !*  sfc: Water held at the surface. [kg m-2].
    !*  sl: Water held in soil. [kg m-2].
    !*  lz: Water held in an aquifer or lower zone storage. [kg m-2].
    !*  dz: Water held in deep zone storage. [kg m-2].
    !*  lost: Water lost. [kg m-2].
    type, extends(storage_state) :: water_storage
        real(kind = 4), dimension(:), allocatable :: cnpy, sfc, sl, lz, dz, lost
    end type

end module
