AC_INIT([OpenCL], [0.0.2])
AC_CONFIG_SRCDIR([OpenCL.cabal])
AC_PROG_CC

AC_CHECK_HEADERS([CL/cl.h], [], [AC_CHECK_HEADERS([OpenCL/OpenCL.h], [], [AC_MSG_ERROR(could not find OpenCL headers)])])

AC_OUTPUT