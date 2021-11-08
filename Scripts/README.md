## Modelling notes

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
