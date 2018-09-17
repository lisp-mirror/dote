dnl dawn of the Era: a tactical game.
dnl Copyright (C) 2015  cage

dnl This program is free software: you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation, either version 3 of the License, or
dnl (at your option) any later version.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.

dnl You should have received a copy of the GNU General Public License
dnl along with this program.  If not, see <http://www.gnu.org/licenses/>.

AC_INIT([dote],[0.0.1],[cage-dev@twistfold.it],[dote])

AM_INIT_AUTOMAKE([-Wall foreign])

AM_GNU_GETTEXT([external])

AC_PATH_PROG([LISP_COMPILER],[sbcl],[no])

if [[ "$LISP_COMPILER" = "no" ]]; then
   AC_MSG_ERROR([Can not find SBCL, Common Lisp compiler.])

fi

AC_PATH_PROG([CURL],[curl],[no])

if [[ "$CURL" = "no" ]]; then
   AC_MSG_ERROR([Can not find curl.])
   exit 1;
fi

AC_PATH_PROG([GPG],[gpg],[no])

if [[ "$GPG" = "no" ]]; then
   AC_MSG_ERROR([Can not find gpg, crypto software.])
   exit 1;
fi

AC_PATH_PROG([SDL_CONFIG],[sdl2-config],[no])

if [[ "$SDL_CONFIG" = "no" ]]; then
   AC_MSG_ERROR([Can not find the sdl2 configuration script.])
   exit 1;
fi

AC_PROG_MKDIR_P

AC_SUBST([CFLAGS],[$CFLAGS\ `sdl2-config --cflags`])

AC_SUBST([CFLAGS],[$CFLAGS\ -I/usr/include/GL])

AC_SUBST([LDFLAGS],[$LDFLAGS\ `sdl2-config --libs`])

AC_CHECK_HEADER("SDL_mixer.h", [],[AC_MSG_ERROR([Can not find developement header.])], [])

AC_CHECK_HEADER("gl.h", [],[AC_MSG_ERROR([Can not find developement header.])], [])

AC_CONFIG_FILES([Makefile quick_quicklisp.sh po/Makefile.in])
AC_OUTPUT
