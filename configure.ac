AC_PREREQ(2.68)
AC_INIT([orb], 4.1, neunhoef@mcs.st-and.ac.uk)
AC_CONFIG_SRCDIR([src/orb.c])
AC_CONFIG_HEADER([src/orbconfig.h:src/orbconfig.h.in])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])

AC_PREFIX_DEFAULT([${PWD}])

AC_PROG_CC

## Find out what the best compiler flags are
AX_CC_MAXOPT

AC_LIBTOOL_DLOPEN
AC_DISABLE_STATIC
AC_PROG_LIBTOOL

AC_FIND_GAP

AC_CHECK_SIZEOF([void *])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
