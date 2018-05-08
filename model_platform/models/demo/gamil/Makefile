NETCDFINC   :=  -I/opt/netCDF-intel13-without-hdf5/include
NETCDFLIB   :=  -L/opt/netCDF-intel13-without-hdf5/lib -lnetcdff -lnetcdf

gamil: gamil.f90
	mpiifort -o $@ gamil.f90 $(NETCDFLIB) $(NETCDFINC) -g -traceback -check all -heap-arrays -fp-stack-check
