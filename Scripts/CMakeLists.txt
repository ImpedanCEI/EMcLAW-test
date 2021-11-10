# DIM 2
# MPI ON
# OMP OFF

if (EMcLAW_DIMS STREQUAL 2)
    set(_emclaw_dim_sources
            Src_${EMcLAW_DIMS}d/EM_${EMcLAW_DIMS}d.F90
            Src_${EMcLAW_DIMS}d/EM_polarizations_${EMcLAW_DIMS}d.F90
            Src_${EMcLAW_DIMS}d/EM_sources_${EMcLAW_DIMS}d.F90
            Src_${EMcLAW_DIMS}d/EM_tagging_${EMcLAW_DIMS}d.F90)

    # List of source files
    set(_sources DEFINES.H init_src_pol.cpp Prob.f90)
    list(APPEND _sources ep_mu_${EMcLAW_DIMS}d.F90 force_tagging_${EMcLAW_DIMS}d.F90 metallic_material_${EMcLAW_DIMS}d.F90)

    # List of input files
    set(_input_files inputs probin)

    setup_test(_sources _emclaw_dim_sources _input_files HAS_FORTRAN_MODULES)

    unset(_sources)
    unset(_input_files)
    unset(_emclaw_dim_sources)
endif ()
