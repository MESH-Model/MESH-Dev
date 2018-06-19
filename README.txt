To compile SA_MESH, run 'make' with one of the following targets:
Serial (sa_mesh):
  - make gfortran
  - make ifort
  - mingw32-make mingw_static
Parallel (mpi_sa_mesh; requires OpenMPI compiler):
  - make mpi_gcc
  - make mpi_intel

If no target is specified, 'gfortran' is assumed.

The makefile will automatically remove *.o and *.mod files if the compile process is successful.

Recommended for Cygwin, Ubuntu (without Intel license):
  - sa_mesh: make
  - mpi_sa_mesh: make mpi_gcc
Recommended for Linux where the Intel license is available:
  - sa_mesh: make ifort
  - mpi_sa_mesh: make mpi_intel

Description of target to compiler:
  - gfortran: GNU compiler (gcc and gfortran).
  - ifort: Intel compiler (ifort).
  - mingw_static: MinGW MS-Windows/MS-DOS GNU distribution.
  - mpi_gcc*: GNU version of OpenMPI compiler (mpifort).
  - mpi_intel*: Intel version of OpenMPI compiler (mpifort).

*'mpi_gcc' and 'mpi_intel' are not interchangeable, even though in both cases the compiler is named 'mpifort'.

'debug' target:
  - debug: Add debug flags and options; overrides optimization.
Examples:
  - Compile 'sa_mesh' using gfortran/gcc compiler with debug flags/options:
    make debug
  - Compile 'sa_mesh' using ifort compiler with debug flags/options:
    make ifort debug

'clean' target:
  - clean*: Removes *.o and *.mod files (e.g., after running make with 'debug').
*For MinGW, DIST=mingw must also be provided.
Examples:
  - Remove *.o and *.mod files in Cygwin/Linux environment:
    make clean
  - Remove *.o and *.mod files in MS-Windows/MS-DOS environment:
    mingw32-make clean DIST=mingw

'veryclean' target:
  - veryclean*: Removes *.o and *.mod files, and compiled program.
*For MinGW, DIST=mingw must also be provided.
*For 'mpi_gcc' and 'mpi_intel', MPI=ompi must also be provided.
Examples:
  - Remove *.o and *.mod files, and 'sa_mesh' in Cygwin/Linux environment:
    make veryclean
  - Remove *.o and *.mod files, and 'mpi_sa_mesh' in Cygwin/Linux environment:
    make veryclean MPI=ompi
  - Remove *.o and *.mod files, and 'sa_mesh_static' in MS-Windows/MS-DOS environment:
    mingw32-make veryclean DIST=mingw

Explicit options:
DIST: Compiler family/distribution.
  - Blank/undefined to use GNU/GCC compiler (default).
  - 'intel' to use Intel compiler.
  - 'mingw' to use GNU/GCC compiler; overrides 'rm' with 'del' for MS-Windows/MS-DOS environment.
MPI: Parallel/serial compilation.
  - Blank/undefined to compile in serial (default).
  - 'ompi' to compile using OMPI compiler.
LSS: Land surface scheme (LSS).
  - Blank/undefined to include default versions of CLASS+SVS (default).
ROUTE: Routing scheme.
  - Blank/undefined to include default versions of WF_ROUTE,SA_RTE+RTE (default).
DEBUG: Debugging flags and options.
  - Blank/undefined to compile with 'o2' optimization (default).
  - 'yes' to include debug options and disable compiler optimization.
Examples:
  - Compile 'mpi_sa_mesh' using Intel version of mpifort compiler:
    make DIST=intel MPI=ompi
  - Compile 'sa_mesh' using gfortran/gcc compiler with debug flags/options:
    make DEBUG=yes
