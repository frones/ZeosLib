{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{ Constant property names used by all connections and     }
{ other utilities on core and plain levels                }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2017 Zeos Development Group       }
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

unit ZConnProperties;

interface

{$I ZCore.inc}

{ WARNING! Some of the parameter values are used directly in DBC API, so they
  must not be changed. }

{ Types of parameters:
    BOOLEAN - 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0 in any case to enable, any other
      value to disable (StrToBoolEx is used to convert)
    INT     - number
    STR     - string }

const
  { Parameters common for all DBC's }

  // Type: STR
  // Same as User property
  ConnProps_UID = 'UID';
  ConnProps_Username = 'username';
  // Type: STR
  // Same as Password property
  ConnProps_PWD = 'PWD';
  ConnProps_Password = 'password';
  // Type: STR
  // Same as LibraryLocation property, path to client lib
  ConnProps_LibLocation = 'LibLocation';
  // Type: STR, like CP_UTF8
  // Codepage to interact with driver
  ConnProps_CodePage = 'codepage';
  // Type: BOOLEAN
  // Same as AutoEncodeStrings property
  ConnProps_AutoEncodeStrings = 'AutoEncodeStrings';
  // Type: CP_UTF16 | CP_UTF8 | GET_ACP
  // Same as ControlsCodePage property
  ConnProps_ControlsCP = 'controls_cp';
  // Type: INT
  // The login timeout to use in seconds.
  ConnProps_Timeout = 'timeout';
  // Type: STR
  // Format to display date, like YYYY-MM-DD
  ConnProps_DateDisplayFormat = 'DateDisplayFormat';
  // Type: STR
  // Format to read date
  ConnProps_DateReadFormat = 'DateReadFormat';
  // Type: STR
  // Format to write date
  ConnProps_DateWriteFormat = 'DateWriteFormat';
  // Type: STR, like HH:MM:SS
  // Format to display time
  ConnProps_TimeDisplayFormat = 'TimeDisplayFormat';
  // Type: STR
  // Format to read time
  ConnProps_TimeReadFormat = 'TimeReadFormat';
  // Type: STR
  // Format to write time
  ConnProps_TimeWriteFormat = 'TimeWriteFormat';
  // Type: STR
  // Format to display date & time
  ConnProps_DateTimeDisplayFormat = 'DatetimeDisplayFormat';
  // Type: STR
  // Format to read date & time
  ConnProps_DateTimeReadFormat = 'DatetimeReadFormat';
  // Type: STR
  // Format to write date & time
  ConnProps_DateTimeWriteFormat = 'DatetimeWriteFormat';
  // Type: STR
  // Sets TZAbstractDatabaseInfo.IdentifierQuotes property, refer to Zeos manual for details
  ConnProps_IdentifierQuotes = 'identifier_quotes';

  { Parameters specific to a single DBC }
  
{$IFDEF ENABLE_MYSQL}
  // Type: STR
  // Refer to MySql manual for details
  ConnProps_Datadir = '--datadir';
  // Type: STR
  // Path to library
  ConnProps_Library = 'Library';
{$ENDIF}

implementation

end.
