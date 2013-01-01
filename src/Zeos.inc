{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

// Compilation directives for Lazarus
{$IFDEF FPC}
 {$I ZeosLazarus.inc}
{$ELSE}
 {$DEFINE DELPHI}
  // Defines that go for ALL still supported Delphi's   (usually means they were under ifndef fpc)
 {$DEFINE WITH_DBCONSTS}        // Delphi DBCONSTS, FPC DBCONST
 {$DEFINE WITH_ASLARGEINT}      // tlargeint aslargeint. Fixed 2.6.0+ or 2.6.2+ probably
 {$DEFINE WITH_ASCURRENCY}      // Has AsCurrency.
 {$DEFINE WITH_ASVARIANT}
 {$DEFINE WITH_IPROVIDER}       // Turn on IProviderSupport interface
 {$DEFINE WITH_CHECKFIELDCOMPATIBILITY}
 {$DEFINE WITH_WIDEMEMO}        // Have WIDEMEMO,  For Delphi we turn these off if not applicable
 {$DEFINE WITH_FTWIDESTRING}    // Have ftwidestring, For Delphi we turn these off if not applicable
 {$DEFINE WITH_WIDEFIELDS}	// Wide-fields are supported (common define for ftWideString/ftWideMemo)
 {$DEFINE WITH_DBTABLES}        // Have the BDE DBTables Unit
 {$DEFINE WITH_INLINE_ANSICOMPARETEXT}
{$ENDIF}

// Compilation directives for Delphi4
// Not supported since Zeoslib 7.X
{$IFDEF VER120}
   "Delphi versions below Delphi 7 aren't supported anymore"
{$ENDIF}

// Compilation directives for Delphi 5
// Not supported since Zeoslib 7.X
{$IFDEF VER130}
        "Delphi versions below Delphi 7 aren't supported anymore"
{$ENDIF}

// Compilation directives for Delphi 6
// Not supported since Zeoslib 7.X
{$IFDEF VER140}
    "Kylix3, CBuilder/Delphi Versions below version 7 aren't supported anymore"
{$ENDIF}

// Compilation directives for Delphi 7
{$IFDEF VER150}
  {$DEFINE VER150BELOW} // Used in zeos.inc only
{$ENDIF}

// Compilation directives for Delphi 8
{$IFDEF VER160}
// No directives needed
{$ENDIF}

// Compilation directives for Delphi 9 (DELPHI 2005)
{$IFDEF VER170}
// No directives needed
{$ENDIF}

// Compilation directives for BDS 2006 (Delphi 2006, BCB 2006)
{$IFDEF VER180}
  {$DEFINE BDS4_UP} // Used in code
{$ENDIF}

// Compilation directives for BDS 2007 (Delphi 2007, BCB 2007)
{$IFDEF VER185}
  {$DEFINE BDS4_UP} // Used in code
{$ENDIF}

// Compilation directives for Delphi 2009
{$IFDEF VER200}
  {$DEFINE VER200BELOW} // Used in code
  {$DEFINE DELPHI12_UP} // Used in code
  {$DEFINE BDS4_UP} // Used in code
  {$DEFINE BDS5_UP} // Used in code
  {$DEFINE WITH_INLINE_ANSISTRLCOMP}
{$ENDIF}

// Compilation directives for Delphi 2010
{$IFDEF VER210}
  {$DEFINE VER200BELOW} // Used in code
  {$DEFINE DELPHI12_UP} // Used in code
  {$DEFINE DELPHI14_UP} //Egonhugeist used in my code
  {$DEFINE BDS4_UP} // Used in code
  {$DEFINE BDS5_UP} // Used in code
{$ENDIF}

// Compilation directives for Delphi XE
{$IFDEF VER220}
  {$DEFINE VER200BELOW} // Used in code
  {$DEFINE DELPHI12_UP} // Used in code
  {$DEFINE DELPHI14_UP} //Egonhugeist used in my code
  {$DEFINE DELPHI15_UP} // Used in zeos.inc only
  {$DEFINE BDS4_UP} // Used in code
  {$DEFINE BDS5_UP} // Used in code
{$ENDIF}
{$IFDEF LINUX}
  {$DEFINE UNIX}
{$ENDIF}

// Compilation directives for Delphi XE2
{$IFDEF VER230}
  {$DEFINE VER200BELOW} // Used in code
  {$DEFINE DELPHI12_UP} // Used in code
  {$DEFINE DELPHI14_UP} //Egonhugeist used in my code
  {$DEFINE DELPHI15_UP} // Used in zeos.inc only
  {$DEFINE DELPHI16_UP} // Used in code
  {$DEFINE BDS4_UP} // Used in code
  {$DEFINE BDS5_UP} // Used in code
{$ENDIF}

// Compilation directives for Delphi XE3
{$IFDEF VER240}
  {$DEFINE VER200BELOW} // Used in code
  {$DEFINE DELPHI12_UP} // Used in code
  {$DEFINE DELPHI14_UP} //Egonhugeist used in my code
  {$DEFINE DELPHI15_UP} // Used in zeos.inc only
  {$DEFINE DELPHI16_UP} // Used in code
  {$DEFINE DELPHI17_UP} // Used in code
  {$DEFINE BDS4_UP} // Used in code
  {$DEFINE BDS5_UP} // Used in code
{$ENDIF}
// END of per Delphi version defines

{$IFDEF BDS4_UP}
  {$DEFINE WITH_WIDESTRINGS_GETFIELDNAMES} //overload of GetFieldNames(List: TWideStrings) does exists;
  {$DEFINE WIDE_DATASET}                // TWidedataset exists
  {$DEFINE WITH_PWIDECHAR_TOWIDESTRING} // Able to pass pwidechar to a widestring parameter
  {$DEFINE WITH_IPROVIDERWIDE}          // IProvider -W widestring using interface
  {$DEFINE WITH_WIDESTRUTILS}           // WideStrUtils Unit.
  {$DEFINE WITH_REPORTMEMORYLEAKSONSHUTDOWN} //ReportMemoryLeaksOnShutdown property does exists (testsuites)
{$ENDIF}

{$IFDEF DELPHI12_UP}
  {$DEFINE WITH_TBOOKMARK}              // Have TBookmark
  {$DEFINE WITH_TRECORDBUFFER}          // Abstract buffer type (pansichar or pbyte)
  {$DEFINE WITH_STSTRINGUNICODE}        // ststring is unicodestring.
  {$DEFINE WITH_NEWTOBJECT}             // allows new methods in TObject (D2009+/FPC 2.4.4)
  {$DEFINE WITH_CHARINSET}              // D2009's poor substitute for character specific "xx in yy"
  {$DEFINE WITH_FTSHORTINT}             // ftShortInt is supported
  {$DEFINE WITH_FTBYTE}                 // ftByte is supported
  {$DEFINE WITH_FTEXTENDED}             // ftExtended is supported
  {$DEFINE WITH_FTLONGWORD}             // ftLongWord is supported
  {$DEFINE WITH_ASBYTES}                //  AsBytes is supported
  {$DEFINE WITH_RAWBYTESTRING}          // Have RawByteString CP $FFFFF
  {$DEFINE WITH_DEFAULTSYSTEMCODEPAGE}  // determine the AnsiString CP
  {$DEFINE WITH_UNITANSISTRINGS}        // Have unit AnsiStrings
  {$UNDEF WITH_INLINE_ANSICOMPARETEXT}  // AnsiCompareText is no more inline declared
  {$UNDEF WITH_IPROVIDERWIDE}           // IProvider -W widestring using interface is deprecated since UNICODE and wo't be called anymore
{$ENDIF}

{$IFDEF DELPHI15_UP}
  {$DEFINE WITH_FORMATSETTINGS}
  {$DEFINE WITH_UNICODEFROMLOCALECHARS}
  {$IFDEF WIN64}
    {$UNDEF WITH_DBTABLES} //BDE unit DBTables (TestSuites) isn't supported on x64
  {$ENDIF}
{$ENDIF}

{$IFDEF VER150BELOW}
  {$UNDEF WITH_WIDEMEMO}
  {$UNDEF WITH_FTWIDESTRING}    // D7 doesn't support TField.AsWideString
  {$UNDEF WITH_WIDEFIELDS}      // absolutely no TWide-Fields support
{$ENDIF}

{$IFDEF DELPHI16_UP}
  {$DEFINE WITH_SYSTEMCLASSES}          //Need System.Classes
  {$DEFINE WITH_MAXLISTSIZE_DEPRECATED} //Since XE2 64Bit Integer = Int64 this value is deprecated
{$ENDIF}

{$IFDEF DELPHI17_UP}
  {$UNDEF WIDE_DATASET}                // TWideDataset and TDataSet are merged together: https://forums.embarcadero.com/thread.jspa?messageID=499588
  {$DEFINE WITH_TVALUEBUFFER}          // SetFieldData/GetFieldData and Parameters using TValueBuffer instead of Pointer
  {$DEFINE WITH_BOOKMARKDATA_TBOOKMARK}
  {$UNDEF WITH_WIDESTRINGS_GETFIELDNAMES} //GetFieldNames with TWideStrings has been removed
{$ENDIF}
// Debug/release compiler options
{$D+}

//force 4 byte enumerations. We are interfacing with c libraries !!
{$Z+}

{$IFOPT D-}
{$DEFINE BINARY_RELEASE}
{$ENDIF}

{$IFDEF BINARY_RELEASE}
// Settings for Release mode
{$C-}
{$I-}
{$R-}
{$L-}
{$Q-}
{$IFNDEF FPC}
{$O+}
{$W-}
{$ENDIF}

{$ELSE}

// Settings for Debug mode
{.$C+}
{.$I+}
{.$R+}
{.$L+}
{.$Q+}
{.$IFNDEF FPC}
{.$O-}
{.$W+}
{.$ENDIF}

{$ENDIF}

// Disables checking code.
// Use Range checking option to turn on/off optimization
{$IFOPT R-}
{$DEFINE DISABLE_CHECKING}
{$ENDIF}


// Enables MySQL support in TZConnection/TZDataset
{$IFNDEF ZEOS_DISABLE_MYSQL}
{$DEFINE ENABLE_MYSQL}
{$ENDIF}

// Enables PostgreSQL support in TZConnection/TZDataset
{$IFNDEF ZEOS_DISABLE_POSTGRESQL}
{$DEFINE ENABLE_POSTGRESQL}
{$ENDIF}

// Enables Sybase/MSSQL support in TZConnection/TZDataset
{$IFNDEF ZEOS_DISABLE_DBLIB}
{$DEFINE ENABLE_DBLIB}
{$ENDIF}

// Enables ADO support in TZConnection/TZDataset
{$IFNDEF ZEOS_DISABLE_ADO}
{$IFNDEF FPC}
 {$IFNDEF UNIX}
  {$DEFINE ENABLE_ADO}
 {$ENDIF}
{$ENDIF}
{$ENDIF}

// Enables Interbase/Firebird support in TZConnection/TZDataset
{$IFNDEF ZEOS_DISABLE_INTERBASE}
{$DEFINE ENABLE_INTERBASE}
{$ENDIF}

// Enables SQLite support in TZConnection/TZDataset
{$IFNDEF ZEOS_DISABLE_SQLITE}
{$DEFINE ENABLE_SQLITE}
{$ENDIF}

// Enables Oracle support in TZConnection/TZDataset
{$IFNDEF ZEOS_DISABLE_ORACLE}
{$DEFINE ENABLE_ORACLE}
{$ENDIF}

// Enables ASA support in TZConnection/TZDataset
{$IFNDEF ZEOS_DISABLE_ASA}
{$DEFINE ENABLE_ASA}
{$ENDIF}

// Enables Pooled connection support for all enabled db drivers in TZConnection/TZDataset
{$IFNDEF ZEOS_DISABLE_POOLED}
{$DEFINE ENABLE_POOLED}
{$ENDIF}

// Supported language. Now available languages:
// ENGLISH, GERMAN, PORTUGUESE, DUTCH, SPANISH, ROMANA, INDONESIAN, RUSSIAN, CZECH, POLISH
{$DEFINE ENGLISH}

// Prevents loading default libmysql.dll
{.$DEFINE MYSQL_STRICT_DLL_LOADING}

// Prevents loading default firebird.dll
{.$DEFINE FIREBIRD_STRICT_DLL_LOADING}

// Prevents loading default libpq.dll
{.$DEFINE POSTGRESQL_STRICT_DLL_LOADING}

//Allows to see SQL exceptions as strings
{$DEFINE INTERBASE_EXTENDED_MESSAGES}

// Loads libcrypt.so before Firebird client library.
// It fixes error "Undefined symbol: crypt".
{$DEFINE INTERBASE_CRYPT}

// Excludes old ZeosDBO from the performance tests
{$DEFINE EXCLUDE_OLD_ZEOS_TEST}

// Excludes DBExpress from the performance tests
{$DEFINE EXCLUDE_DBX_TEST}

// Excludes IBX from the performance tests
{$DEFINE EXCLUDE_IBX_TEST}

// Excludes BDE from the performance tests
{$DEFINE EXCLUDE_BDE_TEST}

// Registers property editors for the components.
{$DEFINE WITH_PROPERTY_EDITOR}


// Turn on multithreading
{$DEFINE MULTI_THREADED}

// In Version 6.1.5 there are several bugs with the TZSQLMetadata-Component
// For Version 6.5.0 you should uncomment the following line
{$DEFINE USE_METADATA}

//Default. To validate an optimal updatecount of 1. Comment this define if this
//dafault behavior isn't wanted. On the other hand you can suppress this
//validation by adding the line ValidateUpdateCount=-1 or FALSE or OFF into the
// TZDataSet-Component.Properties(TStrings)
{$DEFINE WITH_VALIDATE_UPDATE_COUNT}

// A large database may have many tables, colums and/or procedures!!!
// Therefore there is the ability to show a warning
// before retrieving a list of these database objects
{.$DEFINE SHOW_WARNING}

// Use libfbclient.so under linux
// and not libfbembed.so when Firebird 1.5.2 is used under linux
{$DEFINE USELIBFBCLIENTSO}

// Use SynEdit for the SQL Editors.
// Uncomment it when you will use SynEdit instead of a MemoEdit.
// The SynEdit component must be installed on your system.
// You can get SynEdit at http://synedit.sourceforge.net/
{.$DEFINE USE_SYNEDIT}

// Compile test applications with a graphical user interface (GUI)
// Comment this definition if you want to compile console test applications
{.$DEFINE TESTGUI}

