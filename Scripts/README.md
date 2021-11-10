## Modelling notes

Each EMcLAW test have the same files:
**- GNUmakefile:** here you can choose the number of dimensions, MPI, OMP, profiling and debugging.

**-Make.package:** the files to be compiled. This file should not be modified by an user.

**-inputs:** here an user can control when a simulation ends, the boundary conditions, the cfl, some refinement and regridding conditions and the plotfiles.

**-probin:** you can change the automatic tagging depending on the energy density values or gradients.

**-DEFINES.H:** here you can choose different automatic taggings depending on the permittivities, the polarization or if you want to permit forced tagging or not.
You can here enable metallic materials, different polarizations and divergence control. You can also control if some variables are shown or not in the plots. When this file is changed you should type make clean and then make again to allow the changes to be done properly.

**-init_src_pol.cpp:** here the sources and the polarizations can be altered. With the polarizations you should change the DEFINES.H file accordingly.

**-Prob.f90:** initial conditions are set here.

**-ep_mu_(1/2/3)d.F90:** the permittivities can be altered here. Some common geometries have short ways to be written that can be found in DEFINES.H.

**-metallic_material_(1/2/3)d.F90:** to have perfect metals we need 2 subroutines and both should be changed to make our metals work. In DEFINES.H metallic materials have to be defined.

**-force_tagging_(1/2/3)d.F90:** you can put the mesh refinement wherever you want here.

### Em vector array positions
 
 From EM_setup: 
 ```
    desc_lst.setComponent(State_Type, 0, "Dx", bcDx,
                          StateDescriptor::BndryFunc(emfill));

    desc_lst.setComponent(State_Type, 1, "Dy", bcDy,
                          StateDescriptor::BndryFunc(emfill));

    desc_lst.setComponent(State_Type, 2, "Dz", bcDz,
                          StateDescriptor::BndryFunc(emfill));

    desc_lst.setComponent(State_Type, 3, "Bx", bcBx,
                          StateDescriptor::BndryFunc(emfill));

    desc_lst.setComponent(State_Type, 4, "By", bcBy,
                          StateDescriptor::BndryFunc(emfill));

    desc_lst.setComponent(State_Type, 5, "Bz", bcBz,
                          StateDescriptor::BndryFunc(emfill));

    desc_lst.setComponent(State_Type, 6, "Phi", bc,
                          StateDescriptor::BndryFunc(emfill));

    desc_lst.setComponent(State_Type, 7, "Psi", bc,
                          StateDescriptor::BndryFunc(emfill));

    desc_lst.setComponent(State_Type, 8, "epx", bc,
                          StateDescriptor::BndryFunc(emfill));

    desc_lst.setComponent(State_Type, 9, "epy", bc,
                          StateDescriptor::BndryFunc(emfill));

    desc_lst.setComponent(State_Type, 10, "epz", bc,
                          StateDescriptor::BndryFunc(emfill));

    desc_lst.setComponent(State_Type, 11, "mux", bc,
                          StateDescriptor::BndryFunc(emfill));

    desc_lst.setComponent(State_Type, 12, "muy", bc,
                          StateDescriptor::BndryFunc(emfill));

    desc_lst.setComponent(State_Type, 13, "muz", bc,
                          StateDescriptor::BndryFunc(emfill));
	```
