

This is my fork of imapget.  


TODO

read the cafile location from config.txt


Instructions.

HaskellNet is a dependency, but use the fork from
http://github.com/danchoi/HaskellNet instead of the
one fetched by cabal install to get it to install properly.

HsOpenSSL is also a dependency. The official version does not build
successfully on OS X Lion.  You can use
http://github.com/danchoi/HsOpenSSL in that case.
