1 Introduction
══════════════

  Dawn of the Era (dote for short) will be a fantasy, turn based,
  tactical game.


1.1 Features
────────────

  • procedural generation of:
    • levels;
    • terrains;
    • trees;
    • labyrinths;
    • combat units;
    • game object (potions, weapons, etc..);
    • spells;
    • most of the textures.
  • supported mesh in obj and MD2 file format, tags supported for the
    latter;
  • simple caching system for levels;
  • OpenGL GUI (minimal).


2 Current status
════════════════

  The game is *playable* but full of bugs and many things are missing
  (most of the audio and video).  Please send me an email if you want to
  [help].


[help] See section 8


3 Building
══════════

  A primitive building method using autotools is supported.

  1. run
     ┌────
     │ ./configure
     └────
     this script will check all the system dependency (compiler, C
     libraries etc.) are met (see 3.5, 4).  And, if not, will print an
     error.

  2. if `configure' passed in the previous step run:
     ┌────
     │ bash quick_quicklisp.sh
     └────

     this command will download quicklisp, check the signature and
     install all the lisp dependencies.

  3. finally:
     ┌────
     │ make && make install
     └────

  You should be now able to run the game typing `dote' on the terminal.


3.1 If you plan to contribute to the development of this game
─────────────────────────────────────────────────────────────

  1. edit the file src/config.lisp and modify the constants
     `+sys-data-dir+' and `+catalog-dir+' to match the path where the
     sources are.

     For example:

     ┌────
     │ (alexandria:define-constant +sys-data-dir+
     │ "/home/username/lisp/dote/data/" :test #'string=)
     │
     │ (alexandria:define-constant +catalog-dir+
     │ "/home/username/lisp/dote/po" :test #'string=)
     │
     └────

  2. edit the file `dote.asd' and uncomment the line:
     ┌────
     │ ;;(pushnew :debug-mode                        *features*)
     └────
  3. open emacs and slime, load dote and finally, on the REPL type
     (main-window::main-debug).


3.2 Hardware requisite
──────────────────────

  • 8 Gb of RAM;
  • Accelerated GPU. Tested only with Mesa library with Intel(R) HD
    Graphics 2500 GPU.


3.3 Dependency
──────────────

  Of course a Common lisp compiler is mandatory, the compilation has
  been tested only with SBCL version 1.4.10 on Debian GNU/Linux
  (testing).


3.4 Lisp libraries
──────────────────

  dote depends on the following lisp libraries:
  • uiop (bundled with ASDF3);
  • swank;
  • clunit2;
  • alexandria;
  • lparallel;
  • parse-number;
  • ieee-floats;
  • flexi-streams;
  • cl-ppcre-unicode;
  • osicat;
  • babel;
  • trivial-garbage;
  • xmls;
  • cl-i18n;
  • marshal;
  • log4cl;
  • sb-cga;
  • cl-opengl;
  • sdl2kit;
  • cl-sd2-mixer;
  • s-dot2;
  • cl-kanren.


3.5 C libraries
───────────────

  There are others native (in C) libraries used, the configure script
  will warn if some of them are missing:

  • libsdl2;
  • sdl2-mixer;
  • An OpenGL implementation (>= 3.3).

  on my system (debian testing) the packages names are:

  • libsdl2-dev;
  • libsdl2-mixer-dev;
  • libgl1-mesa-dev.


4 External programs
═══════════════════

  • bash;
  • curl.
  • gpg;
  • GNU AWK (gawk);
  • sbcl (lisp compiler).


5 Issues
════════

  I strongly recommend you to compile and run the demo in a testing
  environment as it is unstable (i.  e. frequently crash), remember also
  that there is [NO WARRANTY].


[NO WARRANTY] See section 9


6 BUGS
══════

  Please send bug report to cage-dev at twistfold dot it or point your
  browser to the [notabug repository].


[notabug repository] https://notabug.org/cage/dote/


7 License
═════════

  This program is released under GNU General Public license version 3 or
  later (see COPYING file).

  The program use data and code from other sources, please see
  LICENSES.org for credits.

  Altough any efforts has been put to make the list of credits
  exaustive, errors are always possible.  Please send correction to
  cage-dev at twistfold dot it.


8 Contributing
══════════════

  Any help is appreciated.  artists, translators and coders are needed.
  If you intend to contribute please send a message to cage-dev at
  twistfold dot it or, alternatively, point your browser to the [issue
  tracker].


[issue tracker] https://notabug.org/cage/dote/issues


9 NO WARRANTY
═════════════

  Dawn of the era: a tactical game.  Copyright (C) 2015, 2016, 2017,
  2018 cage

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or (at
  your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see [http://www.gnu.org/licenses/].


[http://www.gnu.org/licenses/] http://www.gnu.org/licenses/


10 Thanks
═════════

  • All the peoples listed in LICENSES.org file;
  • Salvatore di Martino for the name "Dawn of the era";
  • Opengameart.org for the great work;
  • ufoai.org for inspiration and documentation on MD2 model.
