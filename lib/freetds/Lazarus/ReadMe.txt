All Lazarus controls assume all strings are UTF8Encoded which the MsSQL Servers do not understand.

So we need a compiled FreeTDS version with libiconv2.dll

Downloadlink: ftp://ftp.freepascal.org/fpc/contrib/windows/

Use TZConnection.LibraryLocation := 'dblib.dll'; to load the right library.

Actually we are looking for somebody who can compile the FreeTDS lib with the StoredProc functions. 
Actually we do NOT support Strored procedures/functions with the FreeTDS library.