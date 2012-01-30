AC_PREREQ(2.68)
LT_PREREQ([2.4.2])
AC_INIT([orb], 4.1, neunhoef@mcs.st-and.ac.uk)
AC_CONFIG_SRCDIR([src/orb.c])
AC_CONFIG_HEADER([src/pkgconfig.h:cnf/pkgconfig.h.in])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([cnf])
AM_INIT_AUTOMAKE([foreign])
AM_MAINTAINER_MODE
LT_INIT([disable-static dlopen win32-dll])

AC_PREFIX_DEFAULT([${PWD}])

AC_PROG_CC

## Find out what the best compiler flags are
AX_CC_MAXOPT

AC_FIND_GAP

AC_CHECK_SIZEOF([void **])

AC_CYGWIN
AM_CONDITIONAL([SYS_IS_CYGWIN], [test "$CYGWIN" = "yes"])
if test "$CYGWIN" = "yes"; then
  AC_DEFINE(SYS_IS_CYGWIN32, 1, are we on CYGWIN?)
else
  AC_DEFINE(SYS_IS_CYGWIN32, 0, are we on CYGWIN?)
fi



case "$host" in
        *-darwin* )
        AC_DEFINE(SYS_IS_DARWIN, 1, are we on DARWIN?)
        ;;
        * )
        AC_DEFINE(SYS_IS_DARWIN, 0, are we on DARWIN?)
        ;;
esac
     
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
