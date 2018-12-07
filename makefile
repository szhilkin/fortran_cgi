# vim: noexpandtab: tabstop=4:

FLIBS=flibs-0.9/flibs/src

FORTRAN=gfortran
FORTRANFLAGS=-ldl -lfcgi -pthread -Wl,-rpath -Wl,/usr/lib

OBJECTS = \
	cgi_protocol.o \
	fcgi_protocol.o \

dart_cgi: dart_cgi.F90 $(OBJECTS)
	$(FORTRAN) -o $@ $^ $(FORTRANFLAGS) 

cgi_protocol.o: $(FLIBS)/cgi/cgi_protocol.f90
	$(FORTRAN) -c $<

fcgi_protocol.o: $(FLIBS)/cgi/fcgi_protocol.f90
	$(FORTRAN) -c $<

clean:
	rm -f -v dart_fcgi *.o *.mod $(FLIBS)/sqlite/*.o

.PHONY: clean
