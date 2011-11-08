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
  {$DEFINE VER130BELOW}
  {$DEFINE VER140BELOW} 

  // We assume 2 levels is sufficient. 
  // The higher the third number in a version, the better... (we hope) 
  {$IFDEF  VER1_0} 
    {$FATAL Please use FPC 2.0 or later to compile this!} 
  {$ENDIF} 

  {$IFDEF FPC_FULLVERSION}  // FPC_FULLVERSION was added in FPC 2.2.4 
    {$DEFINE FPC2_2UP} 

    {$MACRO ON}             // Macros needed for FPC_VERSION and FPC_RELEASE 

    {$IF FPC_VERSION = 2} 
      {$IF FPC_RELEASE = 3} 
        {$DEFINE FPC2_3UP} 
      {$ENDIF} 
      {$IF FPC_RELEASE = 4} 
        {$DEFINE FPC2_4UP} 
      {$ENDIF} 
      {$IF FPC_RELEASE >= 5} 
        {$DEFINE FPC2_5UP} 
      {$ENDIF} 
    {$ENDIF} 

    {$IF FPC_VERSION > 2} 
      {$DEFINE FPC2_5UP} 
    {$ENDIF} 

  {$ELSE} 
    {$IFDEF VER_2_1} {$DEFINE FPC2_1UP} {$ENDIF} 
    {$IFDEF VER_2_2} {$DEFINE FPC2_2UP} {$ENDIF}      // 2.2.0 - 2.2.3 
  {$ENDIF} 

  {$IFDEF FPC2_5UP} {$DEFINE FPC2_4UP} {$ENDIF} 
  {$IFDEF FPC2_4UP} {$DEFINE FPC2_3UP} {$ENDIF} 
  {$IFDEF FPC2_3UP} {$DEFINE FPC2_2UP} {$ENDIF} 
  {$IFDEF FPC2_2UP} {$DEFINE FPC2_1UP} {$ENDIF} 
  {$IFDEF FPC2_1UP} {$DEFINE FPC2_UP}  {$ENDIF} 
