Contents of this file
=====================

This zip-file contains the FreeTDS dblib implementation in the three flavours
32 bits, 32 bits with a statically linked libiconv and 64 bits. They are
provided here for Zeos users that don't want to compile their own libs.

If you need anything else go to www.freetds.org, get the source and compile
FreeTDS yourself. Instructions on how to do that can be found on the Zeoslib
Wiki:
https://sourceforge.net/p/zeoslib/wiki/Building%20FreeTDS%20with%20libiconv%20on%20Windows%2032%20Bits/

Although the files are named sybdb.dll they really can communbicate to Sybase
servers as well as Microsoft SQL Server.

The 32 bits build and the 64 bits builöd only support the character sets
ISO 8859-1 and UTF-8 only. With Zeoslib you most probably want to use UTF-8.

The 32 bits build that has a statically linked libiconv supports any character
set that is supported by libiconv. You probably want to use that build with the
ANSI Delphi Versions Delphi 2007 and before.

All of these builds depend on the MSVCR120.DLL. So if you use them you will
need the Visual C++ 2013 Redistribnutable.

License
=======
FreeTDS is licensed under the GNU LGPL license. See license.txt for details.