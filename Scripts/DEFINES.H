
/* Tagging defines */
#define TAGGING_SOURCES   
#define TAGGING_EP_MU    
#define TAGGING_POLARIZATIONS     
#define FORCE_TAGGING     
/*FORCE_TAGGING can be used to tag metals*/

/* Metallic materials */
#define METALLIC_MATERIALS

/* Polarization defines */
#define N_POL_KERR (0)      
#define N_POL_LORENTZ (0)   
#define N_POL_HN (0)   
#define N_POL_ION (0)   
#define N_POLARIZATIONS (N_POL_KERR+N_POL_LORENTZ+N_POL_HN) 

/* Force custom source
#define FORCE_CUSTOM_SOURCE   

#define CUSTOM_SOURCE                                              \
   if (time.le.12.5) then;                                         \
      uout(i) = sin(4.0*3.141592*time)*cos(0.5*3.141592*time);     \
   endif;*/

/* Divergences control */
/*#define DIV_CONTROL
#define SHOW_DIV_D
#define SHOW_DIV_B*/

#ifdef METALLIC_MATERIALS
#undef DIV_CONTROL
#endif

/* Plot electric field E */
#define SHOW_E
/* Plot magnetic field H */
#define SHOW_H
/* Plot Poynting vector  */
#define SHOW_POYNTING

/*Materials geometry defines*/
#define CIRCLE_2D(px,py,R,epsilon,mu_)                \
      if ((x-px)**2 + (y-py)**2 <= R**2) then;        \
         ep(i,j,0) = epsilon;                         \
         ep(i,j,3) = sqrt(epsilon);                   \
         ep(i,j,1) = epsilon;                         \
         ep(i,j,4) = sqrt(epsilon);                   \
         ep(i,j,2) = epsilon;                         \
         ep(i,j,5) = sqrt(epsilon);                   \
         mu(i,j,0) = mu_;                             \
         mu(i,j,3) = sqrt(mu_);                       \
         mu(i,j,1) = mu_;                             \
         mu(i,j,4) = sqrt(mu_);                       \
         mu(i,j,2) = mu_;                             \
         mu(i,j,5) = sqrt(mu_);                       \
      endif;

#define SPHERE_3D(px,py,pz,R,epsilon,mu_)             \
      if ((x-px)**2+(y-py)**2+(z-pz)**2 <= R**2) then;\
         ep(i,j,k,0) = epsilon;                         \
         ep(i,j,k,3) = sqrt(epsilon);                   \
         ep(i,j,k,1) = epsilon;                         \
         ep(i,j,k,4) = sqrt(epsilon);                   \
         ep(i,j,k,2) = epsilon;                         \
         ep(i,j,k,5) = sqrt(epsilon);                   \
         mu(i,j,k,0) = mu_;                             \
         mu(i,j,k,3) = sqrt(mu_);                       \
         mu(i,j,k,1) = mu_;                             \
         mu(i,j,k,4) = sqrt(mu_);                       \
         mu(i,j,k,2) = mu_;                             \
         mu(i,j,k,5) = sqrt(mu_);                       \
      endif;

#define SQUARE_2D(pxlo,pxhi,pylo,pyhi,epsilon,mu_)    \
      if ((y.ge.pylo).and.(y.le.pyhi).and.(x.ge.pxlo).and.(x.le.pxhi)) then;        \
         ep(i,j,0) = epsilon;                         \
         ep(i,j,3) = sqrt(epsilon);                   \
         ep(i,j,1) = epsilon;                         \
         ep(i,j,4) = sqrt(epsilon);                   \
         ep(i,j,2) = epsilon;                         \
         ep(i,j,5) = sqrt(epsilon);                   \
         mu(i,j,0) = mu_;                             \
         mu(i,j,3) = sqrt(mu_);                       \
         mu(i,j,1) = mu_;                             \
         mu(i,j,4) = sqrt(mu_);                       \
         mu(i,j,2) = mu_;                             \
         mu(i,j,5) = sqrt(mu_);                       \
      endif;

#define CUBE_3D(pxlo,pxhi,pylo,pyhi,pzlo,pzhi,epsilon,mu_)             \
      if ((z.ge.pzlo).and.(z.le.pzhi).and.(y.ge.pylo).and.(y.le.pyhi).and.(x.ge.pxlo).and.(x.le.pxhi)) then;        \
         ep(i,j,k,0) = epsilon;                         \
         ep(i,j,k,3) = sqrt(epsilon);                   \
         ep(i,j,k,1) = epsilon;                         \
         ep(i,j,k,4) = sqrt(epsilon);                   \
         ep(i,j,k,2) = epsilon;                         \
         ep(i,j,k,5) = sqrt(epsilon);                   \
         mu(i,j,k,0) = mu_;                             \
         mu(i,j,k,3) = sqrt(mu_);                       \
         mu(i,j,k,1) = mu_;                             \
         mu(i,j,k,4) = sqrt(mu_);                       \
         mu(i,j,k,2) = mu_;                             \
         mu(i,j,k,5) = sqrt(mu_);                       \
      endif;

#define ELLIPSE_2D(px,py,a,b,epsilon,mu_)         \
      if (((x-px)**2/(a**2) + (y-py)**2/(b**2)).le.1.0d0) then;        \
         ep(i,j,0) = epsilon;                         \
         ep(i,j,3) = sqrt(epsilon);                   \
         ep(i,j,1) = epsilon;                         \
         ep(i,j,4) = sqrt(epsilon);                   \
         ep(i,j,2) = epsilon;                         \
         ep(i,j,5) = sqrt(epsilon);                   \
         mu(i,j,0) = mu_;                         \
         mu(i,j,3) = sqrt(mu_);                   \
         mu(i,j,1) = mu_;                         \
         mu(i,j,4) = sqrt(mu_);                   \
         mu(i,j,2) = mu_;                         \
         mu(i,j,5) = sqrt(mu_);                   \
      endif;

#define ELLIPSE_3D(px,py,pz,a,b,c,epsilon,mu_)         \
      if (((x-px)**2/(a**2) + (y-py)**2/(b**2) + (z-pz)**2/(c**2)).le.1.0d0) then;        \
         ep(i,j,k,0) = epsilon;                         \
         ep(i,j,k,3) = sqrt(epsilon);                   \
         ep(i,j,k,1) = epsilon;                         \
         ep(i,j,k,4) = sqrt(epsilon);                   \
         ep(i,j,k,2) = epsilon;                         \
         ep(i,j,k,5) = sqrt(epsilon);                   \
         mu(i,j,k,0) = mu_;                         \
         mu(i,j,k,3) = sqrt(mu_);                   \
         mu(i,j,k,1) = mu_;                         \
         mu(i,j,k,4) = sqrt(mu_);                   \
         mu(i,j,k,2) = mu_;                         \
         mu(i,j,k,5) = sqrt(mu_);                   \
      endif;
