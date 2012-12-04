Compiling FreeTDS DB-Lib with MS Visual C++ 2005/2008/2010:
===========================================================
1.   Download sources from www.freetds.org
2.   Open FreeTDS.dsw from /win32/msvc6 source directory
2.1  in libTDS / Header Files edit config.h and comment "HAVE_INTTYPES_H":
     /* #undef HAVE_INTTYPES_H */
     (http://www.freetds.org/userguide/osissues.htm#WINDOWS)
2.2  In Build / Configuration Manager select "Release"
     Right-click on project "dblib_dll" and select "Properties". Note: select Configuration Properties first if in Visual C++ 2010.
     C/C++ / Preprocesor / Preprocessor Definitions add "MSDBLIB" (optionally default TDS version "TDS71")
     Linker / Input / Additional Dependencies add ".\tds_Release\libTDS.lib"
     Linker / General / Output File change from ".\dbdll_Release\dblib_dll.dll" to ".\dbdll_Release\dblib.dll"
3.   Build "dblib_dll"
4.   The dblib.dll will appear in the .\dbdll_Release\ subdirectory
     Note: To avoid dependency on msvc*.dll you can set in C/C++ / Code Generation / Runtime Library : "Multi-threaded (/MT)" in all projects


Compiling FreeTDS with iconv support:
=====================================
(not required when you don't use char/varchar/text datatypes or if you use character set (SBCS) ISO-8859-1 (Latin1) for your char/varchar/text columns)
1.  Download libiconv developer files and binaries for Windows from http://gnuwin32.sourceforge.net/packages/libiconv.htm
	Setup program: http://gnuwin32.sourceforge.net/downlinks/libiconv.php
	- or -
	Developer files: http://gnuwin32.sourceforge.net/downlinks/libiconv-lib-zip.php (include/iconv.h and lib/libiconv.lib)
	Binaries: http://gnuwin32.sourceforge.net/downlinks/libiconv-bin-zip.php  (bin/libiconv2.dll)
    and extract them to a directory, e.g. the directory iconv below your root FreeTDS folder	
2.  in libTDS / Header Files edit config.h and uncomment /* #undef HAVE_ICONV */:
    #define HAVE_ICONV 1
3.  in Project properties:
    libTDS: C/C++ / General / Additional Include Directories add path to "include/iconv.h" (e.g. "..\..\iconv\src\libiconv\1.9.2\libiconv-1.9.2\include"
    dblib_dll: Linker / Input / Additional Dependencies add "lib/libiconv.lib" (e.g. "..\..\iconv\lib\libiconv.lib"
4.  Follow regular compilation instructions above
5.  Distribute libiconv2.dll with your dblib.dll



Known problems:
===============
- CHAR/VARCHAR data truncated to column length when encoding to UTF-8 (use NCHAR/NVARCHAR instead or CAST char/varchar to nchar/nvarchar)
- Multiple result sets (for example when SP returns more than 1 result set only 1st is processed)
- DB-Library error 10038 "Results Pending" - set PacketRecords=-1 to fetch all pendings rows
- BLOB data (IMAGE/TEXT columns) larger than 16MB are truncated to 16MB - (set TZConnection.Properties: 'TEXTSIZE=2147483647' or execute 'SET TEXTSIZE 2147483647')
  (create temporary stored procedures for prepared statements)


Manuals for DB-Library API:
===========================
http://msdn.microsoft.com/en-us/library/aa936988(v=sql.80).aspx
http://manuals.sybase.com/onlinebooks/group-cnarc/cng1110e/dblib/
