# CFLAGS= -I"C:\Program Files\Borland\BCPP551\include" -L"C:\Program Files\Borland\BCPP551\lib" -pr -O2 -C -K -N- -k- -d -3 -r- -u-
# 
# CFLAGS= -I"C:\Program Files\Borland\BCPP551\include" -L"C:\Program Files\Borland\BCPP551\lib" -u- -v -pr -O2 -k- -d -3 -r-
CFLAGS=-I"C:\program files\borland\BCC551\include" -L"C:\program files\borland\BCC551\lib" -p -O1 -C -K -N- -k- -d -3 -r- -u-
INCLUDE=-I"C:\program files\borland\BCC551\include" -L"C:\program files\borland\BCC551\lib"

goal: pcre.obj study.obj pcreposix.obj get.obj

dftables.exe: dftables.c makefile
  @bcc32 $(INCLUDE) dftables.c

chartables.c: dftables.exe makefile
  @dftables.exe >chartables.c
  
pcre.obj: chartables.c pcre.c config.h internal.h pcre.h makefile
	@bcc32 -c $(CFLAGS) pcre.c

study.obj: study.c config.h internal.h pcre.h makefile
	@bcc32 -c $(CFLAGS) study.c

pcreposix.obj: pcreposix.c makefile
  @bcc32 -c $(CFLAGS) pcreposix.c

get.obj: get.c makefile
  @bcc32 -c $(CFLAGS) get.c

clean:
  @if exist *.exe del *.exe
  @if exist *.obj del *.obj
  @if exist chartables.c del chartables.c
