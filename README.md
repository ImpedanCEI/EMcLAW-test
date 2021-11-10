# Installation
### Install AMReX

Type in the folder you want to install AMREX:
```
git clone https://github.com/AMReX-Codes/amrex.git
```
Download the tutorials in the Tutorials/ folder using
```
git clone https://github.com/AMReX-Codes/amrex-tutorials
```
Set the AMREX installation folder as environment variable using in bash:
```
export AMREX_HOME=/path/to/amrex
```
AMREX documentation can be accessed through sphynx module online:
https://amrex-codes.github.io/amrex/docs_html/.
or locally generate them typing `make html` inside the `amrex/Docs/sphinx_documentation` directory. Then convert to a pdf if desired using make latexpdf (latexmk module needs to be installed

### run AMReX examples in local

You build the code in the `amrex/Tutorials/Basic/HelloWorld_C/` directory. Typing `make` will start the compilation process and result in an executable named `main3d.gnu.DEBUG.ex.`

The example code can be run as follows,

`./main3d.gnu.DEBUG.ex`

The result may look like,
```
AMReX (17.05-30-g5775aed933c4-dirty) initialized
Hello world from AMReX version 17.05-30-g5775aed933c4-dirty
AMReX (17.05-30-g5775aed933c4-dirty) finalized
```

For more elaborated tests, you may have to use an input file. For example o build a 2D executable, go to `amrex/Tutorials/Basic/HeatEquation_EX1_C/Exec` and type `make DIM=2`. This will generate an executable named `main2d.gnu.ex.` To run it, type:
```
./main2d.gnu.ex inputs
```
Note that the command takes a file inputs. The calculation solves the heat equation in 2D on a domain with 256x256 cells. It runs 10,000 steps and makes a plotfile every 1000 steps. When the run finishes, you will have a number of plotfiles, plt00000, plt01000, etc, in the directory where you are running. You can control runtime parameters such as how many time steps to run and how often to write plotfiles by setting them in inputs.

### Install EMcLAW
EMcLAW is a Maxwell's equation solver including AMR, polarization, perfect metals and
divergence control. To obtain it from the bitbucket repository type: 
```
cd cd /path/to/amrex/Tutorials/
git clone https://bitbucket.org/emfield/emclaw/src/master/
```
The whole code can be used just pasting it inside AMReX (amrex-master/Tutorials/Amr)
as if it was an extra tutorial. 
All the tests and tutorials, the ones in EMcLAW and the ones in AMReX, are easily compiled just typing `make`. To run the executable just type `./executable_name inputs` (you can use mpirun too). 
To visualize the plots and to work with the data there are several alternatives like VisIt or yt (for python).

### Install Visit
The generated plotfiles can be visualized easily with the opensource code VisIt
Download both the tgz file and the visit-install file in the desired folder from
https://visit-dav.github.io/visit-website/releases-as-tables/#latest 

To convert visit-install to an executable type:
```
chmod 755 visit-install3_2_1
```
Then to install type:
```
./visit-install3_2_1 "version" "platform" "directory"
```
-- "version" will be 3.2.1 for the current distribution.
-- "platform" can be for Ubuntu20.4 linux-x86_64-ubuntu20. The one you use should match the name of the accompanying distribution file.
-- "directory" is the directory you wish to install visit into.

Add the bin directory below the installation directory (/usr/local/visit/bin in our example) to each user's search path.
```
 cd
 echo "set path = ($path /usr/local/visit/bin)" >> .bash_profile
 ```
 Run visit typing: `./visit`
 
 One can also add `visit` to path by typing: `export PATH=/home/edelafue/Visit/bin:$PATH` Then each time we type visit in the terminal, visit frontend will launch.
 
 To open a file directly on visit, type: `visit -o pltXXXX/Header`
 
 To erase all unwanted plt* files type `rm -r plt*`
 

