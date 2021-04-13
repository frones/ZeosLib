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

{$IFDEF INTEL_ASM} //just used for PG actually
{$asmmode intel}
{$ENDIF}

{$DEFINE PUREPASCAL}

{$DEFINE WITH_VARIANT_UINT64}         // System.TVariant supports UInt64 Values
{$DEFINE WITH_VARIANT_UNICODESTRING}  // System.TVariant supports UnicodeString Values
{$DEFINE NO_CONST_ZEROBCD}  // FmtBcd.ZeroBcd is a operator not a const
{$DEFINE WITH_TFIELD_FREEBUFFERS}     //TField has virtual FreeBuffers procedure
{$DEFINE TFIELD_HAS_ASBYTES}          // TField.AsBytes is supported
{$DEFINE TFIELD_HAS_ASLARGEINT}       // TField-baseclass hase AsLargeInt Property
{$DEFINE HAVE_UNKNOWN_CIRCULAR_REFERENCE_ISSUES} //seems FPC behaves different to Delphi. Some weired ref issues arrive if Ancastor units using Descendants in implementation uses section??
{$DEFINE NO_PUBLISHED_RECORDS_ALLOWED} //the compiler is not able to publish records using RTTI
{$DEFINE HAVE_BCDTOSTR_FORMATSETTINGS} //(Try)BCDToStr/StrToBCD has parameter FormatSettings
{$DEFINE NEED_TYPED_UINT64_CONSTANTS}
{$DEFINE WITH_PUREPASCAL_INTPOWER}
{$DEFINE TFIELD_ASWIDESTRING_IS_WIDESTRING}
{$DEFINE HAVE_CS_TRYENTER} //TCriticalSection has method TryEnter
{$DEFINE WITH_REGISTER_CLASSES_BUG}// FPC is not able to handle non visual component classes(supported since D5) except using RegisterNoIcon method
{$DEFINE HAVE_INSERT_BOOKMARK_BUG} //FPC does set bfEOF on Append but on Insert a row the Bookmark-Flag is bfCurrent while it should be bfInsert min v3.1
{$DEFINE WITH_UNIDIRECTIONALBUG} //still buggy with IsUnidirectional prop of TDataSet FPC still moves the recordBuffer-> visit TestReadOnlyQueryUniDirectional and TZAbstractRODataset.GetRecord
{$IFNDEF CPU386}
  {$DEFINE WITH_IE200706094} //Fatal: Internal error 200706094 see: http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=11096#p34381
{$ENDIF}
// These are in a spearate file as Delphi compilers don't digest the macros stuff very well
{$MACRO ON}             // Macros needed for FPC_VERSION and FPC_RELEASE
{$MODE DELPHI}
  
{$INTERFACES COM}
{$IFDEF FPC_FULLVERSION}  // FPC_FULLVERSION was added in FPC 2.2.4
  {$DEFINE WITH_DATASET_DefaultBufferCount} //the fpc aquires min DefaultBufferCount of rows on internalopen/resync, etc. See dataset.inc
  {$DEFINE WITH_PARAM_ASLARGEINT} //supportet since FPC 2.0.4 -> see http://zeoslib.sourceforge.net/viewtopic.php?p=68484#p68484
  {$DEFINE WITH_FTWIDESTRING}  // Supported from somewhere between 2.2 and 2.4
  {$DEFINE WITH_PARAM_ASWIDESTRING}

  {$DEFINE WITH_ACTIVE_DATASET_ERROR_ON_FIRST_ROW_BUG} // FPC datasets are activ on open a query but first next errors out

  {$DEFINE WITH_FPC_FTTIME_BUG} //resolved since 2.6.2
  {$DEFINE WITH_FPC_BOF_BUG} //resolved since 2.7
  {$DEFINE WITH_FPC_MODIFIED_BUG} //resolved since 2.7 http://bugs.freepascal.org/view.php?id=22381

  {$IF FPC_FULLVERSION>=20500}  // Upgrade to 2.6.x asap.
    {$DEFINE WITH_NEWTOBJECT}
    {$DEFINE WITH_IPROVIDER}
    {$DEFINE HAVE_UNICODESTRING}
    {$DEFINE WITH_PWIDECHAR_TOWIDESTRING}
    {$DEFINE WITH_CLASS_VARS}
    {$IF FPC_FULLVERSION>=20600}  // assumes 2.5.1 pretty close to 2.6.0. Summer 2011 or newer
      {$DEFINE TOBJECT_HAS_TOSTRING}            // TObject has a virtual ToString method
      {$DEFINE WITH_INLINE}           // compiler supports inline methodes
      {$DEFINE WITH_FUNIDIRECTIONAL} // FPC has references a field instead of a setter here.  (at least in 2.6.0)
      {$DEFINE WITH_CLASS_CONST} //compiler supports contants in class type declarations 
      {$DEFINE WITH_INC_PBYTE_SUPPORT}  // Compiler supports incrementing PByte-Pointer
      {$DEFINE WITH_WIDEMEMO}
      {$DEFINE WITH_ASLARGEINT}      // tlargeint aslargeint. Fixed 2.6.0+ or 2.6.2+ probably
      {$DEFINE WITH_ASCURRENCY}      // Has AsCurrency.
      {$DEFINE WITH_ASVARIANT}
      {$DEFINE WITH_FORMATSETTINGS}
      {$DEFINE WITH_WIDEFIELDS}      // Wide-fields are supported (common define for ftWideString/ftWideMemo)
      {$DEFINE PWIDECHAR_IS_PUNICODECHAR}
      {$DEFINE WITH_FTGUID} //TStringField descendant TGuidField is supported
      {$DEFINE WITH_VIRTUAL_TFIELD_ASWIDESTRING}
      {$IF FPC_FULLVERSION>20601}
        {$DEFINE WITH_DEPRECATED_MESSAGE}  //Compiler supports deprecated messages
        {$UNDEF WITH_FPC_FTTIME_BUG} //seems like the never FPC2.6.2 also have fixed the NativData bug for TTimeFields+TDataSet
        {$IF FPC_FULLVERSION>20700}       // will be introduced in 2.7+
          {$UNDEF WITH_FPC_BOF_BUG}
          {$UNDEF WITH_FPC_MODIFIED_BUG}
          {$ifndef MSEgui}
            {$DEFINE WITH_TBOOKMARK}        // Have TBookmark
          {.$ELSE}
            {.$DEFINE UNICODE} //16bit UTF16-Encoded see https://zeoslib.sourceforge.io/viewtopic.php?t=4688
          {$endif}
          {$DEFINE TFIELD_HAS_ASBYTES}          // TField.AsBytes is supported
          {$DEFINE TFIELD_HAS_ASLARGEINT}  // TFIeld.AsLargeInt is supported
          {$DEFINE WITH_RAWBYTESTRING}          // Have RawByteString CP $FFFFF
          {$DEFINE WITH_FPC_STRING_CONVERSION} //After string conversions to RawByteString like AnsiToUTF8 the results are equal to original value
          {$DEFINE WITH_DEFAULTSYSTEMCODEPAGE}
          {$DEFINE MISS_RBS_SETSTRING_OVERLOAD} //SetString overload for RawByteString is missing
          {$IFNDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //Hack for *nix systems where i couldn't locate this define )):
            {$DEFINE FPC_HAS_BUILTIN_WIDESTR_MANAGER}
          {$ENDIF}
          {$DEFINE WITH_RAWBYTESTRING_CONVERSION_BUG} //2.7 gen did bad conversions with possible data-loss here
          {$IF FPC_FULLVERSION>=30000}
            {$DEFINE WITH_TESTCASE_ERROR_ADDRESS} //testcase methods like Fail() have error address pointer
            {$DEFINE TENCODING_HAS_CODEPAGE}
            {$DEFINE WITH_WITH_TFIELD_BIND_METHOD} //TField.Bind(Binding) is supported
            {$DEFINE WITH_PARAM_ASBCD} //TParam.AsBcd is supported
            {$DEFINE WITH_tagVARIANT_UINT64} //ActiveX tagVariant supports Int64/uInt64 values not sure about the proper FPC version here
            {$DEFINE WITH_varUString} //Variant now supports refcounted UnicodeStrings
            {$UNDEF WITH_RAWBYTESTRING_CONVERSION_BUG} //issue seems to be fixed
            {$DEFINE WITH_VIRTUAL_TFIELD_BIND} //TField has procedure Bind(Binding: Boolean);
            {$IF FPC_FULLVERSION>=30100}
              {$UNDEF MISS_RBS_SETSTRING_OVERLOAD} //SetString overload for RawByteString is available now
              {$DEFINE WITH_NOT_INLINED_WARNING} //handle marked as inline is not inlined warnings
              {$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined} //suppress warning that marks all Variant operators f.ex.
              {$DEFINE WITH_ASANSISTRING} // TField.AsAnsiString is supported
              {$DEFINE WITH_VIRTUAL_TFIELD_ASUTF8STRING} // //TField has virtual getter+Setter for the UTF8Strings
              {$DEFINE WITH_VIRTUAL_TFIELD_ASUNICODESTRING} //TField has virtual getter+Setter for the UnicodeStrings
              {$IF FPC_FULLVERSION>=30200}
                {$UNDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}
                {$IF (defined(CPU64) or defined(CPUARM)) and not defined(ENDIAN_BIG)} 
                  {$DEFINE WITH_PG2DOUBLE_INLINE_BUG} //wrong inlining results to wrong double values on PG binary wire
                {$IFEND}  
                {$DEFINE WITH_VAR_INIT_WARNING}
                {$DEFINE WITH_CASE_WARNING} //Since FPC 3.2 we've some weird warnigns on using case X of
                {$DEFINE WITH_CODEPAGE_AWARE_FIELD}
                {$DEFINE TBLOBDATA_IS_TBYTES} //TBlobData is an alias for TBytes instead of AnsiString
                {$DEFINE WITH_TSTRINGS_TENCODING_LOADFROM}           // TStrings.LoadFromFile/LoadFromStream have TEncoding
                {$IF FPC_FULLVERSION>=30300}
                  {$DEFINE WITH_FTLONGWORD}                 // ftLongWord is supported
                  {$DEFINE WITH_FTSHORTINT}                 // ftShortInt is supported
                  {$DEFINE WITH_FTBYTE}                     // ftByte is supported
                  {$DEFINE WITH_FTEXTENDED}                 // ftExtended is supported
                {$IFEND} 
              {$IFEND} 
            {$IFEND}
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$ELSE}
      {$FATAL Versions Older than 2.6.0 are not supported!}
    {$IFEND}
  {$IFEND}
{$ELSE !FPC_FULLVERSION}
  {$FATAL Versions Older than 2.6.0 are not supported!}
{$ENDIF}

  // todo
  //   VIRTUALSETFIELDDATA: in nativeformat versions of SETFIELDDATA, inherited wasn't called.
  //                Afaik that methoid is virtual in FPC since December 2006, and there is no comment why
  //                it was ifdefed. We'll leave it off, and see if sb complains.
  //   LAZARUSUTF8HACK :  Places that fixed for the Lazarus convention that all strings are UTF8  are marked with this
  //                             ifdef. But probably most of them are adhoc hacks, and need constant reevaluation.
  //   OLDFPC      : IFDEF FPC's so old that I can't remember which version are put under IFDEF OLDFPC. In time these
  //                   blocks can be killed.

{$IFDEF LCL}
  {$DEFINE WITH_LCONVENCODING} //Only available for Lazarus
{$ENDIF}

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
