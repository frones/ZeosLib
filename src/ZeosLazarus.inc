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

{$DEFINE PUREPASCAL}

{$DEFINE MISS_VARIANTCLEAR}           //oleaut32 external function VariantClear is missing
{$DEFINE WITH_EARGUMENTEXCEPTION}     // EArgumentException is supported since?????
{.$DEFINE WITH_VARIANT_UINT64}         // System.TVariant supports UInt64 Values
{$DEFINE WITH_VARIANT_UNICODESTRING}  // System.TVariant supports UnicodeString Values
{$DEFINE WITH_TFIELD_FREEBUFFERS} //TField has virtual FreeBuffers procedure
{$DEFINE TFIELD_HAS_ASBYTES}          // TField.AsBytes is supported
{$DEFINE TFIELD_HAS_ASLARGEINT}       // TField-baseclass hase AsLargeIntProperty
{$DEFINE HAVE_UNKNOWN_CIRCULAR_REFERENCE_ISSUES} //seems FPC behaves different to Delphi. Some weired ref issues arrive if Ancastor units using Descendants in implementation uses section??
{$DEFINE NEED_TYPED_UINT64_CONSTANTS}
{$DEFINE WITH_PUREPASCAL_INTPOWER}
{$IFDEF SYSTEMINLINE} //global FPC define
  {$DEFINE WITH_INLINE}           // compiler supports inline methodes
{$ENDIF}
{$DEFINE WITH_UNIDIRECTIONALBUG} //still buggy with IsUnidirectional prop of TDataSet FPC still moves the recordBuffer-> visit TestReadOnlyQueryUniDirectional and TZAbstractRODataset.GetRecord
{$IFNDEF MSWINDOWS}
 {$DEFINE USE_RAW2WIDE_PROCS} //FPC is really not optimal with code generation use MultibultToWidechar instead for known CP's
{$ENDIF}
{$IFNDEF CPU386}
  {$DEFINE WITH_IE200706094} //Fatal: Internal error 200706094 see: http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=11096#p34381
{$ENDIF}
// These are in a spearate file as Delphi compilers don't digest the macros stuff very well
{$MACRO ON}             // Macros needed for FPC_VERSION and FPC_RELEASE
  {$MODE DELPHI}

  {$INTERFACES COM}
  {$IFDEF FPC_FULLVERSION}  // FPC_FULLVERSION was added in FPC 2.2.4
    {$IF FPC_FULLVERSION<=20204}
      {$DEFINE OLDFPC}
    {$IFEND}

//    {$IF FPC_FULLVERSION>=20400}  // 2.4.0 status is unknown. 2.4.2+ should be generally fine.
//      {$DEFINE FPC2_4UP}
//    {$IFEND}
    {$DEFINE WITH_PARAM_ASLARGEINT} //supportet since FPC 2.0.4 -> see http://zeoslib.sourceforge.net/viewtopic.php?p=68484#p68484
    {$DEFINE WITH_FTWIDESTRING}  // Supported from somewhere between 2.2 and 2.4

    {$IF FPC_FULLVERSION>=20500}  // Upgrade to 2.6.x asap.
      {$DEFINE FPC2_5UP} // Used in code
      {$DEFINE WITH_NEWTOBJECT}
      {$DEFINE WITH_IPROVIDER}
    {$IFEND}
    {$IF FPC_FULLVERSION>=20600}  // assumes 2.5.1 pretty close to 2.6.0. Summer 2011 or newer
       {$DEFINE WITH_INC_PBYTE_SUPPORT}  // Compiler supports incrementing PByte-Pointer
       {$DEFINE WITH_WIDEMEMO}
       {$DEFINE WITH_ASLARGEINT}      // tlargeint aslargeint. Fixed 2.6.0+ or 2.6.2+ probably
       {$DEFINE WITH_ASCURRENCY}      // Has AsCurrency.
       {$DEFINE WITH_ASVARIANT}
       {$DEFINE WITH_FORMATSETTINGS}
       {$DEFINE WITH_WIDEFIELDS}      // Wide-fields are supported (common define for ftWideString/ftWideMemo)
       {$DEFINE PWIDECHAR_IS_PUNICODECHAR}
       {$DEFINE WITH_FTGUID} //TStringField descendant TGuidField is supported 
       {$DEFINE FPC2_6UP}
    {$IFEND}

    {$IF FPC_FULLVERSION>=20602}
       {$DEFINE HAVE_TBYTES}         // TBytes is declared only in FPC 2.6.2 instead of 2.6.0 as declared...
    {$ENDIF}

    {$IFDEF LCL}
      {$DEFINE WITH_LCONVENCODING} //Only available for Lazarus
    {$ENDIF}
    {$DEFINE LAZARUSUTF8HACK} // Places that fixed for the Lazarus convention that all strings are UTF8  are marked with this
                              // ifdef. But probably most of them are adhoc hacks, and need revisiting in time.
    {$DEFINE WITH_FPC_FTTIME_BUG} //http://zeos.firmos.at/viewtopic.php?t=3534&postdays=0&postorder=asc&start=0
    {$DEFINE WITH_FPC_BOF_BUG} //http://bugs.freepascal.org/view.php?id=22377
    {$DEFINE WITH_FPC_MODIFIED_BUG} //http://bugs.freepascal.org/view.php?id=22381
    {$DEFINE WITHOUT_VARBYTESASSTRING}
    {$DEFINE WITH_PWIDECHAR_TOWIDESTRING}
    {$DEFINE WITH_CLASS_VARS}
  {$ELSE}
    {$FATAL Versions Older than 2.2.4 are not supported!}
  {$ENDIF}

  {$DEFINE WITH_FUNIDIRECTIONAL} // FPC has references a field instead of a setter here.  (at least in 2.6.0)

  {$IFDEF FPC_FULLVERSION>20601}
    {$UNDEF WITH_FPC_FTTIME_BUG} //seems like the never FPC2.6.2 also have fixed the NativData bug for TTimeFields+TDataSet
  {$ENDIF}
  {$IF FPC_FULLVERSION>20700}       // will be introduced in 2.7+
    {$DEFINE WITH_TRECORDBUFFER}
    {$ifndef MSEgui} 
      {$DEFINE WITH_TBOOKMARK}        // Have TBookmark
    {$endif}
    {$DEFINE TFIELD_HAS_ASBYTES}          // TField.AsBytes is supported
    {$DEFINE TFIELD_HAS_ASLARGEINT}  // TFIeld.AsLargeInt is supported
    {$UNDEF WITH_FPC_BOF_BUG}
    {$UNDEF WITH_FPC_MODIFIED_BUG}
    {$DEFINE WITH_RAWBYTESTRING}          // Have RawByteString CP $FFFFF
    {$DEFINE WITH_FPC_STRING_CONVERSATION} //After string conversations to RawByteString like AnsiToUTF8 the results are equal to original value
    {$DEFINE WITH_DEFAULTSYSTEMCODEPAGE}
    {$DEFINE MISS_RBS_SETSTRING_OVERLOAD} //SetString overload for RawByteString is missing
    {$IFNDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //Hack for *nix systems where i couldn't locate this define )):
      {$DEFINE FPC_HAS_BUILTIN_WIDESTR_MANAGER}
    {$ENDIF}
    {$DEFINE WITH_RAWBYTESTRING_CONVERSION_BUG} //2.7 gen did bad conversions with possible data-loss here
  {$IFEND}
  {$IF FPC_FULLVERSION>30000}       // will be introduced in 3.0+
    {.$UNDEF MISS_RBS_SETSTRING_OVERLOAD} //SetString overload for RawByteString is available now
    {$UNDEF WITH_RAWBYTESTRING_CONVERSION_BUG} //issue seems to be fixed
    {$DEFINE WITH_VIRTUAL_TFIELD_BIND} //TField has procedure Bind(Binding: Boolean);
  {$IFEND}

  // todo
  //   VIRTUALSETFIELDDATA: in nativeformat versions of SETFIELDDATA, inherited wasn't called.
  //                Afaik that methoid is virtual in FPC since December 2006, and there is no comment why
  //                it was ifdefed. We'll leave it off, and see if sb complains.
  //   LAZARUSUTF8HACK :  Places that fixed for the Lazarus convention that all strings are UTF8  are marked with this
  //                             ifdef. But probably most of them are adhoc hacks, and need constant reevaluation.
  //   OLDFPC      : IFDEF FPC's so old that I can't remember which version are put under IFDEF OLDFPC. In time these
  //                   blocks can be killed.

{$IF defined(WITH_LCONVENCODING) and (defined(MSWINDOWS) or defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER))}
  {$UNDEF WITH_LCONVENCODING}
{$IFEND}

{$IFDEF WITH_LCONVENCODING}
  {$IF declared(lcl_fullversion)}
    {$if lcl_fullversion>=1000000} //Lazarus >= 1.0.0 with delphi fix.
      {$DEFINE LCONVENCODING_HAS_CP852_FUNCTIONS}
    {$ifend}
  {$ifend}
{$ENDIF}
