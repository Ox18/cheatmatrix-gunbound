# "Make" file for PCRE using Borland's free command line tools (BCB5.5)
# Author: Paul Whitehead (paul.whitehead@bcs.org.uk)
# Date: April 4th 2001

# Please note: you will need to edit the "config.in" and "pcre.in" files as per
# the NON-UNIX-USE instructions included with PCRE before using this makefile.
# This is because Win32 systems do not come with 'sed' as a standard tool :-(
# For this reason, the "clean" target will NOT remove either config.h or pcre.h

# Syntax for use with Borland's make:
# make -fwin32.mak [target]

BOR_INST_DIR=\borland\bcc55
CC=$(BOR_INST_DIR)\bin\bcc32.exe
LIBEXE=$(BOR_INST_DIR)\bin\tlib.exe
CPPFLAGS=-I$(BOR_INST_DIR)\include
LINKFLAGS=-L$(BOR_INST_DIR)\lib

.c.obj:
    $(CC) $(CPPFLAGS) -c $<

# first target - this will get run by default if no target specified
all: pcre.lib pcreposix.lib dftables.exe pcretest.exe

pcre.lib: get.obj maketables.obj pcre.obj study.obj
    $(LIBEXE) pcre.lib /a get.obj maketables.obj pcre.obj study.obj, pcre.lst

pcreposix.lib: pcreposix.obj
    $(LIBEXE) pcreposix.lib /a pcreposix.obj, pcreposix.lst

dftables.exe: dftables.obj dftables.c
    $(CC) $(CPPFLAGS) $(LINKFLAGS) dftables.c
    dftables.exe > chartables.c

pcretest.exe: pcre.lib pcreposix.lib pcretest.obj pcretest.c
    $(CC) $(CPPFLAGS) $(LINKFLAGS) pcretest.c pcre.lib pcreposix.lib

clean:
    @del dftables.exe
    @del pcretest.exe
    @del pcre.lib
    @del pcreposix.lib
    @del dftables.obj
    @del get.obj
    @del maketables.obj
    @del pcre.obj
    @del pcreposix.obj
    @del pcretest.obj
    @del study.obj
    @del chartables.c
    # .tds Turbo Debugger symbols - remove these to avoid
    # any confusion when debugging
    @del *.tds
    # .lst files are list files showing the contents of the
    # static libraries - they will be re-generated if required
    @del *.lst

dftables.obj: internal.h dftables.c maketables.c
get.obj: internal.h get.c dftables.exe
maketables.obj: internal.h maketables.c dftables.exe
pcre.obj: internal.h pcre.c dftables.exe
pcreposix.obj: internal.h pcreposix.h pcreposix.c dftables.exe
study.obj: internal.h study.c dftables.exe

