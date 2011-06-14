#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([setuid_drv], [VERSION], [BUG-REPORT-ADDRESS])
AC_CONFIG_SRCDIR([priv/setuid_drv.h])
#AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([string.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_TYPE_UID_T

# Checks for library functions.

AC_ERLANG_NEED_ERLC
AC_ERLANG_SUBST_ROOT_DIR
AC_ERLANG_SUBST_LIB_DIR
AC_ERLANG_CHECK_LIB(erl_interface)

AC_CONFIG_FILES([Makefile
                 priv/Makefile])

AC_OUTPUT
