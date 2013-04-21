hbase85
======

hbase85 is a simple library for performing base85 encoding and decoding
in Haskell. It was written in half a day to learn cabal and how to set up
testing in a cabal project.

Features
========
It supports RFC1924 encoding and decoding of ByteStrings without using a
sentinel byte for groups of 5 zero-bytes.

It supports ASCII85 encoding and decoding and will expand the special 'z' byte
to 5 '!' bytes if encountered.

Bugs
====
Currently it does not handle padding correctly when using little endian encoding
and decoding. Strictly speaking this doesn't really matter, as both RFC1924 and
ASCII85 use big endian.

Efficiency
==========
Probably not so impressive, although I won't know until I try to benchmark it.

Which I might do next time I have half a day to do code for fun.

Requirements
===========
It requires vector and bytestring, I am not sure whether it breaks with old
versions or not.

