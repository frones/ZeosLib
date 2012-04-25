{@********************************************************}
{    Copyright (c) 1999-2006 Zeos Development Group       }
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
{   http://zeosbugs.firmos.at (BUGTRACKER)                }
{   svn://zeos.firmos.at/zeos/trunk (SVN Repository)      }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

// Compilation directives for Lazarus
// These are in a spearate file as Delphi compilers don't digest the macros stuff very well
  {$MODE DELPHI}
  {$INTERFACES COM}

  {$IFDEF FPC_FULLVERSION}  // FPC_FULLVERSION was added in FPC 2.2.4
    {$IF FPC_FULLVERSION<=20204}
      {$DEFINE OLDFPC}
    {$IFEND}

    {$MACRO ON}             // Macros needed for FPC_VERSION and FPC_RELEASE

//    {$IF FPC_FULLVERSION>=20400}  // 2.4.0 status is unknown. 2.4.2+ should be generally fine.
//      {$DEFINE FPC2_4UP}
//    {$IFEND}
    {$IF FPC_FULLVERSION>=20500}  // Upgrade to 2.6.x asap.
      {$DEFINE FPC2_5UP} // Used in code
      {$DEFINE WITH_NEWTOBJECT}
      {$DEFINE WITH_IPROVIDER}
    {$IFEND}
    {$IF FPC_FULLVERSION>=20600}  // assumes 2.5.1 pretty close to 2.6.0. Summer 2011 or newer
       {$DEFINE WITH_WIDEMEMO}
       {$DEFINE WITH_ASLARGEINT}      // tlargeint aslargeint. Fixed 2.6.0+ or 2.6.2+ probably
       {$DEFINE WITH_ASCURRENCY}      // Has AsCurrency.
       {$DEFINE WITH_ASVARIANT}
       {$DEFINE WITH_FORMATSETTINGS}
      {$DEFINE FPC2_6UP}
    {$IFEND}

    {$DEFINE LAZARUSUTF8HACK} // Places that fixed for the Lazarus convention that all strings are UTF8  are marked with this
                              // ifdef. But probably most of them are adhoc hacks, and need revisiting in time.
  {$ELSE}
    {$FATAL Versions Older than 2.2.4 are not supported!}
  {$ENDIF}

  {$DEFINE WITH_FUNIDIRECTIONAL} // FPC has references a field instead of a setter here.  (at least in 2.6.0)

  {$IF FPC_FULLVERSION>20600} // will be introduced in 2.6.2 (and up to date 2.6.1)
    {$DEFINE WITH_TRECORDBUFFER}
    {$DEFINE WITH_TBOOKMARK}              // Have TBookmark
  {$IFEND}



  // todo
  //   VIRTUALSETFIELDDATA: in nativeformat versions of SETFIELDDATA, inherited wasn't called.
  //                Afaik that methoid is virtual in FPC since December 2006, and there is no comment why
  //                it was ifdefed. We'll leave it off, and see if sb complains.
  //   LAZARUSUTF8HACK :  Places that fixed for the Lazarus convention that all strings are UTF8  are marked with this
  //                             ifdef. But probably most of them are adhoc hacks, and need constant reevaluation.
  //   OLDFPC      : IFDEF FPC's so old that I can't remember which version are put under IFDEF OLDFPC. In time these
  //                   blocks can be killed.

