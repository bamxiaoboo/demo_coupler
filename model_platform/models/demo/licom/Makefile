NETCDFINC   :=  -I/opt/netCDF-intel13-without-hdf5/include
NETCDFLIB   :=  -L/opt/netCDF-intel13-without-hdf5/lib -lnetcdff -lnetcdf

licom: licom.F90
	mpiifort -o $@ licom.F90 $(NETCDFLIB) $(NETCDFINC) -g -traceback -check all -heap-arrays -fp-stack-check
