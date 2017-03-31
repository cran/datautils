AC_INIT([datautils], [0.1.1], [pbruneau@gmail.com])
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
   echo "could not determine R_HOME"
   exit 1
fi


CC=`"${R_HOME}/bin/R" CMD config CC`
CPP=`"${R_HOME}/bin/R" CMD config CPP`
CFLAGS=`"${R_HOME}/bin/R" CMD config CFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`



AC_SUBST(LIBS)
AC_SUBST(CPPFLAGS)
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

