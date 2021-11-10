
#include <AMReX_MultiFabUtil.H>
#include <EM.H>
#include <EM_F.H>
#include <AMReX_VisMF.H>
#include <AMReX_TagBox.H>
#include <AMReX_ParmParse.H>

#include "DEFINES.H"

using namespace amrex;

// initial conditions, sources and polarizations

void
EM::initData ()
{
    const Real* dx  = geom.CellSize();
    const Real* prob_lo = geom.ProbLo();
    MultiFab& S_new = get_new_data(State_Type);
    int ngrow = S_new.nGrow();
	 int ncomp = S_new.nComp();
    S_new.setVal(0.0, 0, ncomp, ngrow); // Initialization to zero
    Real cur_time   = state[State_Type].curTime();
    // epx,epy,epz,sqrtepx,sqrtepy,sqrtepz 
    // mux,muy,muz,sqrtmux,sqrtmuy,sqrtmuz 
    FArrayBox ep, mu;

    if (verbose && ParallelDescriptor::IOProcessor())
	std::cout << "Initializing the data at level " << level << std::endl;

    for (MFIter mfi(S_new); mfi.isValid(); ++mfi)
    {
        const Box& box     = mfi.validbox();
        const int* lo      = box.loVect();
        const int* hi      = box.hiVect();

   	ep.resize(amrex::grow(box,NUM_GROW),6);
   	mu.resize(amrex::grow(box,NUM_GROW),6);

       get_ep_mu(prob_lo,lo,hi,
		   BL_TO_FORTRAN_3D(S_new[mfi]),
			BL_TO_FORTRAN_3D(ep), 
			BL_TO_FORTRAN_3D(mu), 
		   dx,NUM_GROW,NUM_STATE,is_D_wave,0.0);

    //
    // Loop over grids, call FORTRAN function to init with data.
    //
          initdata(level, cur_time, ARLIM_3D(lo), ARLIM_3D(hi),
		   BL_TO_FORTRAN_3D(S_new[mfi]), ZFILL(dx),
		   ZFILL(prob_lo), NUM_STATE);
    }

   static bool first_entry = true;
   if ( first_entry ) {
   first_entry = false;

   Real delta = 0.1;
   Real xlo = -3.1;
   Real xhi = -2.9;
   Real ylo = -0.1;
   Real yhi = 0.1;
   Real zlo = 0.3;
   Real zhi = 0.4;

   RealBox rb;
   rb.setLo(0, xlo);
   rb.setHi(0, xhi);
   if (BL_SPACEDIM >= 2) {
   rb.setLo(1, ylo);
   rb.setHi(1, yhi);
   }
   if (BL_SPACEDIM == 3) {
    rb.setLo(2, zlo);
    rb.setHi(2, zhi);
   }

   RealBox rb2;
   rb2.setLo(0, -2.5);
   rb2.setHi(0, 3.5);
   if (BL_SPACEDIM >= 2) {
   rb2.setLo(1, -2.5);
   rb2.setHi(1, 2.5);
   }
   if (BL_SPACEDIM == 3) {
    rb2.setLo(2, 0.5);
    rb2.setHi(2, 0.9);
   }

   RealBox rb3;
   rb3.setLo(0, 0.7);
   rb3.setHi(0, 0.9);
   if (BL_SPACEDIM >= 2) {
   rb3.setLo(1, 0.7);
   rb3.setHi(1, 0.9);
   }
   if (BL_SPACEDIM == 3) {
    rb3.setLo(2, 0.7);
    rb3.setHi(2, 0.9);
   }


   //
   // Sources
   //
   SOURCES::C_sources CV_sources;


   Real field_amplitude = 1.0;

   Real wavelength = 1.0;
   const Real freq = 1.0 / wavelength;

   SOURCES::src_time::ptr_src_t psrc_pulse = std::make_shared<SOURCES::n_pulse_src_time>(freq,1,M_PI*0.5,0);
   SOURCES::src_time::ptr_src_t psrc_cont = std::make_shared<SOURCES::continuous_src_time>(freq);
   SOURCES::src_time::ptr_src_t psrc_HHG  = std::make_shared<SOURCES::gaussian_src_time>(freq,3.0,0.0,3.33,0.0);

   CV_sources.add_source(SOURCES::Field_component::f3, rb, field_amplitude, psrc_cont);

   f_sources = CV_sources;


   //
   // Polarizations
   //
   // Respect the order: kerr, lorentz, HN and DEFINES.H must be ok
   //
   POL::C_polarizations CV_polarizations;

   POL::pol_time::ptr_pol_t ppol_kerr = std::make_shared<POL::pol_time>(0.33);
   POL::pol_time::ptr_pol_t ppol_lorentz = std::make_shared<POL::pol_time>(0.0,1.1,0.6,0.2);
//   POL::pol_time::ptr_pol_t ppol_lorentz2 = std::make_shared<POL::pol_time>(0.0,1.1,0.6,0.2,0.0,0.0,0.0,0.0,0.0);
//   POL::pol_time::ptr_pol_t ppol_HN = std::make_shared<POL::pol_time>(0.0,0.0,0.0,0.0,1.0,2.0,0.5,0.5,1.0);

//	CV_polarizations.add_pol(POL::Pol_type::kerr, rb2, ppol_kerr);
//	CV_polarizations.add_pol(POL::Pol_type::kerr, rb3, ppol_kerr);
//	CV_polarizations.add_pol(POL::Pol_type::lorentz, rb3, ppol_lorentz2);

   Polarizations = CV_polarizations;


   }//first_entry

    if (verbose && ParallelDescriptor::IOProcessor())
	std::cout << "Done initializing the level " << level << " data " << std::endl;
}

