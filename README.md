# hjs
Haskell JavaScript Parser

Many examples in the testsuite taken from http://developer.mozilla.org/en/docs/Core_JavaScript_1.5_Reference

This is a development version; don't expect everything to work and to be clear.

Documentation can be found at http://www.haskell.org/haskellwiki/Libraries_and_tools/HJS

Installation

runhaskell.exe Setup.hs configure
runhaskell.exe Setup.hs build

Quick test after build:

dist\build\hjs\hjs.exe .\testsuite\0_helloworld.js

To package up for Hackage (sdist doesn't work)

tar cvf hjs-0.2.tar hjs-0.2
gzip hjs-0.2.tar