{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPlainASADriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_ASA}

uses Classes, ZCompatibility, ZPlainDriver;

{$J+}

{ ***************** Plain API Constants definition **************** }

const
  ASA7_WINDOWS_DLL_LOCATION = 'DBLIB7.DLL';
  ASA7_LINUX_DLL_LOCATION = 'libdblib7.so';
  ASA8_WINDOWS_DLL_LOCATION = 'DBLIB8.DLL';
  ASA8_LINUX_DLL_LOCATION = 'libdblib8.so';
  ASA9_WINDOWS_DLL_LOCATION = 'DBLIB9.DLL';
  ASA9_LINUX_DLL_LOCATION = 'libdblib9.so';
  ASA12_WINDOWS_DLL_LOCATION = 'DBLIB12.DLL';
  ASA12_LINUX_DLL_LOCATION = 'libdblib12.so';
  ASA17_WINDOWS_DLL_LOCATION = 'DBLIB17.DLL';
  ASA17_LINUX_DLL_LOCATION = 'libdblib17.so';


  //sqlerr.h
  //* Warnings *//
  SQLE_NOERROR                          = 0;
  SQLE_NOTFOUND                         = 100;
  SQLE_TRUNCATED                        = 101;
  SQLE_TEMPORARY_TABLE                  = 102;
  SQLE_CANNOT_CONVERT                   = 103;
  SQLE_ROW_UPDATED_WARNING              = 104;
  SQLE_PROCEDURE_COMPLETE               = 105;
  SQLE_COLUMN_VALUE_CHANGED             = 106;
  SQLE_SYNTAX_EXTENSION_WARNING         = 107;
  SQLE_CURSOR_OPERATION_CONFLICT        = 108;
  SQLE_NULL_VALUE_ELIMINATED            = 109;
  SQLE_BACKUP_PAGE_INCOMPLETE           = 110;
  SQLE_CANNOT_EXECUTE_STMT              = 111;
  SQLE_MORE_INFO                        = 112;
  SQLE_INVALID_OPTION_ON_CONNECT        = 113;
  SQLE_CANNOT_PERFORM_CHAR_TRANSLATION  = 114;
  SQLE_UNSUPPORTED_CHARACTER_SET_WARNING = 115;
  SQLE_UNSUPPORTED_LANGUAGE             = 116;
  SQLE_UNSUPPORTED_CHARSET_AND_LANGUAGE = 117;
  SQLE_INVALID_USER_ESTIMATE            = 118;
  SQLE_UNABLE_TO_OPEN_BACKUP_LOG        = 119;
  SQLE_UNKNOWN_OPTION                   = 120;
  SQLE_CURSOR_OPTIONS_CHANGED           = 121;
  SQLE_DATABASE_NEW                     = 123;
  SQLE_CANNOT_CONVERT_LOAD_TABLE        = 124;
  SQLE_INPUT_ILLEGAL_MULTIBYTE_WARNING  = 125;
  SQLE_OUTPUT_ILLEGAL_MULTIBYTE_WARNING = 126;
  SQLE_INPUT_SIMPLE_SUBSTITUTION_WARNING = 127;
  SQLE_OUTPUT_SIMPLE_SUBSTITUTION_WARNING = 128;
  SQLE_ROW_DROPPED_DURING_SCHEMA_UPGRADE = 130;
  SQLE_CANNOT_DUMP_STRING_HISTOGRAM     = 132;
  SQLE_COMPRESSING_ENCRYPTED_DB         = 135;
  SQLE_WARNING                          = 200;
//  SQLE_HLI_MORE_DATA_AVAILABLE          = 400;

  //Errors
  SQLE_NO_DATABASE_FILE                 = -72;
  SQLE_COMMUNICATIONS_UNDERFLOW         = -73;
  SQLE_DATABASE_NOT_ACTIVE              = -74;
  SQLE_START_STOP_DATABASE_DENIED       = -75;
  SQLE_REQUEST_DENIED_NO_DATABASES      = -76;
  SQLE_ALIAS_CLASH                      = -77;
  SQLE_DYNAMIC_MEMORY_EXHAUSTED         = -78;
  SQLE_INVALID_LOCAL_OPTION             = -79;
  SQLE_UNABLE_TO_START_ENGINE           = -80;
  SQLE_INVALID_COMMAND_LINE             = -81;
  SQLE_UNABLE_TO_START_DATABASE         = -82;
  SQLE_DATABASE_NOT_FOUND               = -83;
  SQLE_INVALID_DATABASE                 = -84;
  SQLE_COMMUNICATIONS_ERROR             = -85;
  SQLE_NO_MEMORY                        = -86;
  SQLE_DATABASE_NAME_REQUIRED           = -87;
  SQLE_PROTOCOL_MISMATCH                = -88;
  SQLE_ENGINE_NOT_MULTIUSER             = -89;
  SQLE_ARGUMENT_CANNOT_BE_NULL          = -90;
  SQLE_UNHANDLED_JAVA_EXCEPTION         = -91;
  SQLE_BAD_CLASS_FILE                   = -92;
  SQLE_FIELD_NOT_FOUND                  = -93;
  SQLE_INVALID_FIELD_REFERENCE          = -94;
  SQLE_INVALID_PARSE_PARAMETER          = -95;
  SQLE_ENGINE_ALREADY_RUNNING           = -96;
  SQLE_PAGE_SIZE_TOO_BIG                = -97;
  SQLE_AUTHENTICATION_VIOLATION         = -98;
  SQLE_CONNECTIONS_DISABLED             = -99;
  SQLE_ENGINE_NOT_RUNNING               = -100;
  SQLE_NOT_CONNECTED                    = -101;
  SQLE_TOO_MANY_CONNECTIONS             = -102;
  SQLE_INVALID_LOGON                    = -103;
  SQLE_INVALID_PASSWORD                 = -103;
  SQLE_INVALID_MODULE_LOGON             = -104;
  SQLE_UNABLE_TO_CONNECT                = -105;
  SQLE_CANNOT_OPEN_LOG                  = -106;
  SQLE_ERROR_WRITING_LOG                = -107;
  SQLE_CONNECTION_NOT_FOUND             = -108;
  SQLE_STILL_ACTIVE_CONNECTIONS         = -109;
  SQLE_NAME_NOT_UNIQUE                  = -110;
  SQLE_INDEX_NAME_NOT_UNIQUE            = -111;
  SQLE_EXISTING_PRIMARY_KEY             = -112;
  SQLE_INVALID_FOREIGN_KEY_DEF          = -113;
  SQLE_VIEW_DEFINITION_ERROR            = -114;
  SQLE_MUST_DROP_INDEX                  = -115;
  SQLE_TABLE_MUST_BE_EMPTY              = -116;
  SQLE_PRIMARY_KEY_REFERENCED           = -117;
  SQLE_NO_PRIMARY_KEY                   = -118;
  SQLE_PRIMARY_KEY_COLUMN_DEFINED       = -119;
  SQLE_ALREADY_HAS_GRANT_PERMS          = -120;
  SQLE_PERMISSION_DENIED                = -121;
  SQLE_GROUP_CYCLE                      = -122;
  SQLE_NOT_A_GROUP                      = -123;
  SQLE_TOO_MANY_COLUMNS_DELETED         = -124;
  SQLE_ALTER_CLAUSE_CONFLICT            = -125;
  SQLE_PRIMARY_KEY_TWICE                = -126;
  SQLE_COLUMN_IN_INDEX                  = -127;
  SQLE_USER_OWNS_TABLES                 = -128;
  SQLE_INVALID_STATEMENT                = -130;
  SQLE_SYNTAX_ERROR                     = -131;
  SQLE_STATEMENT_ERROR                  = -132;
  SQLE_INVALID_STATEMENT_TYPE           = -133;
  SQLE_NOT_IMPLEMENTED                  = -134;
  SQLE_LANGUAGE_EXTENSION               = -135;
  SQLE_OUTER_JOIN_CYCLE                 = -136;
  SQLE_CORRELATION_NAME_NEEDED          = -137;
  SQLE_DBSPACE_NOT_FOUND                = -138;
  SQLE_CORRELATION_NAME_AMBIGUOUS       = -139;
  SQLE_UNKNOWN_USERID                   = -140;
  SQLE_TABLE_NOT_FOUND                  = -141;
  SQLE_CORRELATION_NAME_NOT_FOUND       = -142;
  SQLE_COLUMN_NOT_FOUND                 = -143;
  SQLE_COLUMN_AMBIGUOUS                 = -144;
  SQLE_FOREIGN_KEY_NAME_NOT_FOUND       = -145;
  SQLE_CANNOT_JOIN                      = -146;
  SQLE_AMBIGUOUS_JOIN                   = -147;
  SQLE_UNKNOWN_FUNC                     = -148;
  SQLE_INVALID_GROUP_SELECT             = -149;
  SQLE_AGGREGATES_NOT_ALLOWED           = -150;
  SQLE_SUBQUERY_SELECT_LIST             = -151;
  SQLE_INVALID_ORDER                    = -152;
  SQLE_INVALID_UNION                    = -153;
  SQLE_WRONG_PARAMETER_COUNT            = -154;
  SQLE_VARIABLE_INVALID                 = -155;
  SQLE_EXPRESSION_ERROR                 = -156;
  SQLE_CONVERSION_ERROR                 = -157;
  SQLE_OVERFLOW_ERROR                   = -158;
  SQLE_INVALID_COLUMN_NUMBER            = -159;
  SQLE_DESCRIBE_NONSELECT               = -160;
  SQLE_INVALID_DESCRIBE_TYPE            = -161;
  SQLE_CANNOT_OUTER_JOIN                = -162;
  SQLE_NO_COLUMN_NAME                   = -163;
  SQLE_NAMESPACE_HEAP_EXHAUSTED         = -164;
  SQLE_JAVA_VM_HEAP_EXHAUSTED           = -165;
  SQLE_CURSOR_NOT_DECLARED              = -170;
  SQLE_OPEN_CURSOR_ERROR                = -171;
  SQLE_CURSOR_ALREADY_OPEN              = -172;
  SQLE_CURSOR_NOT_OPEN                  = -180;
  SQLE_NO_INDICATOR                     = -181;
  SQLE_SQLDA_TOO_SMALL                  = -182;
  SQLE_INDEX_NOT_FOUND                  = -183;
  SQLE_PUT_CURSOR_ERROR                 = -184;
  SQLE_TOO_MANY_RECORDS                 = -185;
  SQLE_SUBQUERY_RESULT_NOT_UNIQUE       = -186;
  SQLE_CURSOROP_NOT_ALLOWED             = -187;
  SQLE_NOT_ENOUGH_HOST_VARS             = -188;
  SQLE_NOT_FOUND_IN_INDEX               = -189;
  SQLE_NON_UPDATEABLE_COLUMN            = -190;
  SQLE_CANNOT_MODIFY                    = -191;
  SQLE_NON_UPDATEABLE_VIEW              = -192;
  SQLE_PRIMARY_KEY_NOT_UNIQUE           = -193;
  SQLE_INVALID_FOREIGN_KEY              = -194;
  SQLE_COLUMN_CANNOT_BE_NULL            = -195;
  SQLE_INDEX_NOT_UNIQUE                 = -196;
  SQLE_NO_CURRENT_ROW                   = -197;
  SQLE_PRIMARY_KEY_VALUE_REF            = -198;
  SQLE_ONLY_ONE_TABLE                   = -199;
  SQLE_INVALID_OPTION                   = -200;
  SQLE_INVALID_OPTION_SETTING           = -201;
  SQLE_NOT_PUBLIC_ID                    = -202;
  SQLE_TEMPORARY_NOT_ALLOWED            = -203;
  SQLE_OPTION_REQUIRES_DBA              = -204;
  SQLE_INVALID_STANDARD_LOGON           = -205;
  SQLE_INVALID_INTEGRATED_LOGON         = -206;
  SQLE_WRONG_NUM_OF_INSERT_COLS         = -207;
  SQLE_ROW_UPDATED_SINCE_READ           = -208;
  SQLE_INVALID_COLUMN_VALUE             = -209;
  SQLE_LOCKED                           = -210;
  SQLE_MUST_BE_ONLY_CONNECTION          = -211;
  SQLE_CHECKPOINT_REQUIRES_UNDO         = -212;
  SQLE_SUBTRANS_REQUIRE_UNDO            = -213;
  SQLE_TABLE_IN_USE                     = -214;
  SQLE_PROCEDURE_IN_USE                 = -215;
  SQLE_OPTION_IS_TEMP_ONLY              = -216;
  SQLE_OPTION_IN_PROCEDURE              = -217;
  SQLE_AUTHENTICATION_FAILED            = -218;
  SQLE_SQLE_SUBTRANS_NOTFOUND           = -220;
  SQLE_ROLLBACK_NOT_ALLOWED             = -221;
  SQLE_RESULT_NOT_ALLOWED               = -222;
  SQLE_PP_DBLIB_MISMATCH                = -230;
  SQLE_DBLIB_ENGINE_MISMATCH            = -231;
  SQLE_SERVER_ENGINE_MISMATCH           = -232;
  SQLE_UNKNOWN_BACKUP_OPERATION         = -240;
  SQLE_BACKUP_NOT_STARTED               = -241;
  SQLE_BACKUP_CANNOT_RENAME_LOG_YET     = -242;
  SQLE_BACKUP_UNABLE_TO_DELETE_FILE     = -243;
  SQLE_LOG_TRUNCATED                    = -244;
  SQLE_INTEGRATED_LOGON_FAILED          = -245;
  SQLE_INTEGRATED_LOGON_UNSUPPORTED     = -246;
  SQLE_INTEGRATED_LOGON_GUESTMAP        = -247;
  SQLE_INTEGRATED_LOGON_SYSMAP          = -248;
  SQLE_INTEGRATED_LOGON_MAPPED          = -249;
  SQLE_IDENTIFIER_TOO_LONG              = -250;
  SQLE_DUPLICATE_FOREIGN_KEY            = -251;
  SQLE_PRIMARY_KEY_MULTI_ROW_UPDATE     = -252;
  SQLE_PRIMARY_KEY_CURSOR_UPDATE        = -253;
  SQLE_DELETE_SUBQUERY_SAME_TABLE       = -254;
  SQLE_CURSOR_DELETE_SELF_REF           = -255;
  SQLE_INSERT_SELF_REFERENCING          = -256;
  SQLE_VARIABLE_NOT_FOUND               = -260;
  SQLE_VARIABLE_EXISTS                  = -261;
  SQLE_LABEL_NOT_FOUND                  = -262;
  SQLE_INVALID_FETCH_POSITION           = -263;
  SQLE_WRONG_NUM_OF_FETCH_VARIABLES     = -264;
  SQLE_PROCEDURE_NOT_FOUND              = -265;
  SQLE_OLD_DBINIT                       = -266;
  SQLE_ATOMIC_OPERATION                 = -267;
  SQLE_TRIGGER_NOT_FOUND                = -268;
  SQLE_COLUMN_IN_TRIGGER                = -269;
  SQLE_USER_OWNS_PROCEDURES             = -270;
  SQLE_TRIGGER_DEFN_CONFLICT            = -271;
  SQLE_INVALID_TRIGGER_COL_REFS         = -272;
  SQLE_INVALID_TRIGGER_STATEMENT        = -273;
  SQLE_NESTING_TOO_DEEP                 = -274;
  SQLE_PROCEDURES_NOT_IN_DESKTOP        = -275;
  SQLE_PUBLICATION_NOT_FOUND            = -280;
  SQLE_TABLE_HAS_PUBLICATIONS           = -281;
  SQLE_SUBSCRIPTION_NOT_UNIQUE          = -282;
  SQLE_SUBSCRIPTION_NOT_FOUND           = -283;
  SQLE_ONLY_ONE_PUBLISHER               = -284;
  SQLE_NOT_REMOTE_USER                  = -285;
  SQLE_NOT_REMOTE_TYPE                  = -286;
  SQLE_PASSTHROUGH_INCONSISTENT         = -287;
  SQLE_REMOTE_STATEMENT_FAILED          = -288;
  SQLE_CONSOLIDATED_USER_ALREADY_EXISTS = -289;
  SQLE_INVALID_FORMAT_STRING_ARG_NUM    = -294;
  SQLE_CANNOT_UNIQUELY_IDENTIFY_ROWS    = -295;
  SQLE_ERROR_NUMBER_OUT_OF_RANGE        = -296;
  SQLE_USER_DEFINED_EXCEPTION           = -297;
  SQLE_DOUBLE_REQUEST                   = -298;
  SQLE_INTERRUPTED                      = -299;
  SQLE_ERROR                            = -300;
  SQLE_DATABASE_ERROR                   = -301;
  SQLE_TERMINATED_BY_USER               = -302;
  SQLE_DISK_WRITE_FAILED                = -303;
  SQLE_DEVICE_FULL                      = -304;
  SQLE_DEVICE_ERROR                     = -305;
  SQLE_DEADLOCK                         = -306;
  SQLE_THREAD_DEADLOCK                  = -307;
  SQLE_CONNECTION_TERMINATED            = -308;
  SQLE_MEMORY_ERROR                     = -309;
  SQLE_BEYOND_EOF                       = -310;
  SQLE_LOG_CORRUPTED                    = -311;
  SQLE_ALREADY_HAS_GROUP_MEMBERSHIP     = -312;
  SQLE_INTEGRATED_LOGON_UNMAPPED        = -313;
  SQLE_HLI_BAD_SYNTAX                   = -400;
  SQLE_HLI_BAD_CURSOR                   = -401;
  SQLE_HLI_BAD_STATEMENT                = -402;
  SQLE_HLI_BAD_HOST_VAR_NAME            = -403;
  SQLE_HLI_BAD_HOST_VAR_VALUE           = -404;
  SQLE_HLI_BAD_CALLBACK                 = -405;
  SQLE_HLI_INTERNAL                     = -406;
  SQLE_HLI_BAD_ARGUMENT                 = -407;
  SQLE_PHANTOM                          = -501;
  SQLE_UNBLOCKED                        = -502;
  SQLE_PREEMPTED                        = -503;
  SQLE_RETRY                            = -504;
  SQLE_UNSUPPORTED_LOAD_FORMAT          = -601;
  SQLE_CANNOT_ACCESS_FILE               = -602;
  SQLE_COLUMN_VALUE_TOO_LONG            = -603;
  SQLE_DBSPACE_FULL                     = -604;
  SQLE_ACCESS_BEYOND_END_OF_MAX_DBSPACE = -605;
  SQLE_PATTERN_TOO_LONG                 = -606;
  SQLE_CANNOT_STOP_SERVER               = -607;
  SQLE_INVALID_TEXTPTR_VALUE            = -608;
  SQLE_INVALID_TEXT_IMAGE_DATATYPE      = -609;
  SQLE_MESSAGE_ALREADY_EXISTS           = -610;
  SQLE_TSQL_FEATURE_NOT_SUPPORTED       = -611;
  SQLE_MESSAGE_NOT_FOUND                = -612;
  SQLE_USER_TYPE_NOT_FOUND              = -613;
  SQLE_USER_OWNS_MESSAGES_OR_DATATYPES  = -614;
  SQLE_INVALID_PARAMETER_NAME           = -615;
  SQLE_TOO_MANY_COLUMNS_IN_TABLE        = -616;
  SQLE_EXTERNAL_CALLS_NOT_SUPPORTED     = -617;
  SQLE_EXTERNAL_PLATFORM_FAILURE        = -618;
  SQLE_REQUIRE_DLL_NAME                 = -619;
  SQLE_COULD_NOT_LOAD_LIBRARY           = -620;
  SQLE_COULD_NOT_FIND_FUNCTION          = -621;
  SQLE_ERROR_CALLING_FUNCTION           = -622;
  SQLE_DDL_NOT_ALLOWED_IN_PROCEDURES    = -623;
  SQLE_DATATYPE_NOT_ALLOWED             = -624;
  SQLE_TOO_MANY_PARAMETERS              = -625;
  SQLE_THREAD_START_FAILURE             = -626;
  SQLE_INVALID_SYNTAX_EXTENSION         = -627;
  SQLE_DIV_ZERO_ERROR                   = -628;
  SQLE_INVALID_ESCAPE_CHAR              = -629;
  SQLE_INVALID_ESCAPE_SEQ               = -630;
  SQLE_RAISERROR_STMT                   = -631;
  SQLE_WITH_CHECK_OPTION_VIOLATION      = -632;
  SQLE_READ_ONLY_CURSOR                 = -633;
  SQLE_UNTERMINATED_C_STR               = -634;
  SQLE_NO_COLUMN_PERMS_FOR_VIEWS        = -635;
  SQLE_DUPLICATE_REFERENCING_COLUMN     = -636;
  SQLE_DUPLICATE_INSERT_COLUMN          = -637;
  SQLE_STRING_RIGHT_TRUNCATION          = -638;
  SQLE_PARAMETER_NAME_MISSING           = -639;
  SQLE_INVALID_DESCRIPTOR_INDEX         = -640;
  SQLE_ERROR_IN_ASSIGNMENT              = -641;
  SQLE_INVALID_DESCRIPTOR_NAME          = -642;
  SQLE_CANNOT_UNLOAD_A_VIEW             = -643;
  SQLE_PAGE_SIZE_INVALID                = -644;
  SQLE_DATABASE_NOT_CREATED             = -645;
  SQLE_STORE_NOT_LOADED                 = -646;
  SQLE_STORE_ENTRY_NOT_FOUND            = -647;
  SQLE_INVALID_DBSPACE_FOR_CREATE       = -648;
  SQLE_FIELD_CANNOT_BE_NULL             = -649;
  SQLE_INVALID_INDEX_TYPE               = -650;
  SQLE_DROP_DATABASE_FAILED             = -651;
  SQLE_CANNOT_DECOMPRESS_CLASS          = -652;
  SQLE_CLASS_MEMBER_OF_JAR              = -653;
  SQLE_NO_PROFILE_FILE                  = -654;
  SQLE_GEN_PARSE_ERROR                  = -655;
  SQLE_OMNI_CONNECT_ERROR               = -656;
  SQLE_OMNI_NO_RMT_OBJ                  = -657;
  SQLE_OMNI_READONLY                    = -658;
  SQLE_OMNI_SERVER_NOT_FOUND            = -659;
  SQLE_OMNI_REMOTE_ERROR                = -660;
  SQLE_OMNI_BACKWARDS_CURSOR            = -661;
  SQLE_JAVA_SERIALIZATION_ERROR         = -662;
  SQLE_JAVA_DESERIALIZATION_ERROR       = -663;
  SQLE_DATABASE_ACTIVE                  = -664;
  SQLE_DATABASE_NEEDS_RECOVERY          = -665;
  SQLE_OMNI_RMT_TABLE_NOTFOUND          = -666;
  SQLE_OMNI_RMT_COLUMNS_NOTFOUND        = -667;
  SQLE_NO_SCROLL_CURSOR                 = -668;
  SQLE_METHOD_CANNOT_BE_CALLED          = -669;
  SQLE_BAD_CLASS_BYTE_CODE              = -670;
  SQLE_PARAM_NOT_REGISTERED             = -671;
  SQLE_DATABASE_UPGRADE_FAILED          = -672;
  SQLE_DATABASE_UPGRADE_NOT_POSSIBLE    = -673;
  SQLE_INVALID_CURSOR_RANGE             = -674;
  SQLE_JAVA_VM_NOT_STARTED              = -675;
  SQLE_INVALID_TRANSACTION_ISOLATION    = -676;
  SQLE_TABLE_HAS_REFACTION              = -677;
  SQLE_AMBIGUOUS_INDEX_NAME             = -678;
  SQLE_OMNI_MEMORY_CONFIG               = -679;
  SQLE_INVALID_TSQL_OJ_EXPRESSION       = -680;
  SQLE_INVALID_TSQL_JOIN_TYPE           = -681;
  SQLE_OMNI_DEBUG                       = -682;
  SQLE_DUPLICATE_CURSOR_NAME            = -683;
  SQLE_ROLLBACK_ON_PREFETCH             = -684;
  SQLE_RESOURCE_GOVERNOR_EXCEEDED       = -685;
  SQLE_JAVA_VM_INSUFFICIENT_CACHE       = -686;
  SQLE_IQ_PATH_SYNTAX_ERROR             = -687;
  SQLE_NO_ENCRYPTION_IN_RUNTIME         = -688;
  SQLE_BAD_PARAM_INDEX                  = -689;
  SQLE_RETVAL_CANNOT_BE_SET             = -690;
  SQLE_BACKUP_NOT_LOADED                = -691;
  SQLE_BACKUP_ENTRY_NOT_FOUND           = -692;
  SQLE_UNSUPPORTED_JDBC_FEATURE         = -693;
  SQLE_CANNOT_CHANGE_OPENED_STATEMENT   = -694;
  SQLE_JDBC_OBJ_INTERNAL_ERR            = -695;
  SQLE_JDBC_OBJ_CLOSED                  = -696;
  SQLE_BACKUP_ERROR                     = -697;
  SQLE_OMNI_AUTOINC_NOT_SUPPORTED       = -698;
  SQLE_CANNOT_UPDATE_FINAL_FIELD        = -699;
  SQLE_SQLDA_INCONSISTENT               = -700;
  SQLE_CANNOT_ACCESS_INSTANCE_MEMBER    = -701;
  SQLE_CANNOT_TRUNCATE_VIEW             = -702;
  SQLE_COMPUTED_COLUMN_WRITE_ATTEMPTED  = -703;
  SQLE_CANNOT_INDEX_ON_JAVA_CLASS       = -704;
  SQLE_PROCEDURE_RETURNS_VOID           = -705;
  SQLE_OMNI_SERVER_NOT_CAPABLE          = -706;
  SQLE_STMT_NOT_ALLOWED_IN_PASSTHROUGH  = -707;
  SQLE_TEXT_OPERATION_ON_VIEW           = -708;
  SQLE_COMPUTED_COLUMNS_NOT_SUPPORTED   = -709;
  SQLE_INVALID_COMPARISON               = -710;
  SQLE_STORE_VERSION_MISMATCH           = -711;
  SQLE_OMNI_EXTLOGIN_NOT_FOUND          = -712;
  SQLE_JNAT_OBJ_INTERNAL_ERR            = -713;
  SQLE_JNAT_OBJ_CLOSED                  = -714;
  SQLE_RESTORE_INCONSISTENT             = -715;
  SQLE_RESTORE_INVALID_FORMAT           = -716;
  SQLE_RESTORE_UNABLE_TO_OPEN           = -717;
  SQLE_RESTORE_UNABLE_TO_WRITE          = -718;
  SQLE_RESTORE_UNABLE_TO_START          = -719;
  SQLE_CANNOT_VALIDATE_OBJECT           = -720;
  SQLE_OMNI_DATATYPE_MISMATCH           = -721;
  SQLE_OMNI_NOSUCH_COLUMN               = -722;
  SQLE_OMNI_LENGTH_MISMATCH             = -723;
  SQLE_OMNI_NULL_MISMATCH               = -724;
  SQLE_OMNI_IDENTITY_MISMATCH           = -725;
  SQLE_OMNI_RMT_TABLE_NOTUNIQUE         = -726;
  SQLE_CANNOT_OPTIMIZE_QUERY            = -727;
  SQLE_NON_UPDATEABLE_EXT_TAB           = -728;
  SQLE_UNENFORCEABLE_FOREIGN_KEY        = -729;
  SQLE_BAD_JAR_FILE                     = -730;
  SQLE_USER_OWNS_REPLICATED_OBJECTS     = -731;
  SQLE_OMNI_COMPUTED_NOT_SUPPORTED      = -732;
  SQLE_TOO_MANY_NULL_COLUMNS            = -733;
  SQLE_CANNOT_UPDATE_NULL_ROW           = -734;
  SQLE_INVALID_PARAMETER                = -735;
  SQLE_OMNI_UNSUPPORTED_DATATYPE        = -736;
  SQLE_SIGNATURE_MISMATCH               = -737;
  SQLE_PASSWORD_TOO_SHORT               = -738;
  SQLE_DB_INIT_NOT_CALLED               = -739;
  SQLE_FAILED_TO_CREATE_STREAMS_ENV     = -740;
  SQLE_NOTA_WRITE_FILE                  = -741;
  SQLE_JDBC_BATCH_EXECUTE_ABANDONED     = -742;
  SQLE_JDBC_RESULTSET_SEEK_ABSOLUTE_ZERO = -743;
  SQLE_IQ_INVALID_COMMAND_LINE          = -744;
  SQLE_IQ_MEMORY_MANAGER_FAILED         = -745;
  SQLE_IQ_SYSTEM_V_FAILURE              = -746;
  SQLE_JDBC_INVALID_RESULTSET_TYPE      = -747;
  SQLE_JDBC_INVALID_RESULTSET_CONCURRENCY = -748;
  SQLE_NOT_SUPPORTED_IN_ULTRALITE       = -749;
  SQLE_USER_OWNS_PROCEDURES_IN_USE      = -750;
  SQLE_USER_OWNS_TABLES_IN_USE          = -751;
  SQLE_JDBC_INVALID_OPER_ON_INSERT_ROW  = -752;
  SQLE_JDBC_MUST_OPER_ON_INSERT_ROW     = -753;
  SQLE_INVALID_DSN_NAME                 = -754;
  SQLE_UNCOMMITTED_TRANSACTIONS         = -755;
  SQLE_JDBC_TBL_COL_NOT_FOUND_IN_RESULTSET = -756;
  SQLE_READ_ONLY_DATABASE               = -757;
  SQLE_NO_JAVA_SUPPORT                  = -758;
  SQLE_SQLDA_INVALID_DATATYPE           = -759;
  SQLE_INVALID_SQL_IDENTIFIER           = -760;
  SQLE_CAPABILITY_NOT_FOUND             = -761;
  SQLE_NON_PUBLIC_JAVA_CLASS            = -762;
  SQLE_UNKNOWN_JAVA_REF                 = -763;
  SQLE_UNABLE_TO_CONNECT_OR_START       = -764;
  SQLE_NOT_SYNC_TYPE                    = -765;
  SQLE_VIEW_OVER_TEMP_OBJECT            = -766;
  SQLE_SYNCHRONIZATION_NOT_FOUND        = -767;
  SQLE_CANNOT_SUBSCRIBE                 = -768;
  SQLE_CANNOT_MODIFY_SYNC_AS_PUB        = -769;
  SQLE_CANNOT_MODIFY_PUB_AS_SYNC        = -770;
  SQLE_EVENT_NOT_FOUND                  = -771;
  SQLE_EVENT_ALREADY_EXISTS             = -772;
  SQLE_SCHEDULE_NOT_FOUND               = -773;
  SQLE_SCHEDULE_ALREADY_EXISTS          = -774;
  SQLE_CANNOT_SYNC_TABLE_WITHOUT_PK     = -777;
  SQLE_PK_NOT_IN_SYNC_ARTICLE           = -778;
  SQLE_BLOB_IN_SYNC_TABLE_PK            = -779;
  SQLE_ARTICLE_PK_CANNOT_BE_UPDATED     = -780;
  SQLE_SAME_EXCLUSIVE_APP_RUNNING       = -782;
  SQLE_SAME_APP_RUNNING                 = -783;
  SQLE_CANNOT_REG_CONN                  = -784;
  SQLE_DEREG_APP_NOT_FOUND              = -785;
  SQLE_DEREG_APP_IN_USE                 = -786;
  SQLE_CONN_ALREADY_REGISTERED          = -787;
  SQLE_NOT_UNIQUE_CONN_REG_LABEL        = -788;
  SQLE_APP_REG_COOKIE_EXHAUSTED         = -789;
  SQLE_CONN_REG_AUTO_LABEL_EXHAUSTED    = -790;
  SQLE_INVALID_EVENT_DAY                = -791;
  SQLE_INVALID_EVENT_TIME               = -792;
  SQLE_INVALID_EVENT_START              = -793;
  SQLE_UPLOAD_FAILED_AT_SERVER          = -794;
  SQLE_SYNC_TEMPLATE_NOT_FOUND          = -795;
  SQLE_SYNC_SITE_NOT_UNIQUE             = -796;
  SQLE_BAD_SYNC_OPTION_VALUE            = -797;
  SQLE_DTC_TRANSACTIONS_NOT_SUPPORTED   = -799;
  SQLE_CANNOT_COMMIT_OR_ROLLBACK_WHILE_ENLISTED = -800;
  SQLE_CANNOT_ENLIST_WHILE_ALREADY_ENLISTED = -801;
  SQLE_CANNOT_ENLIST_WITH_UNCOMMITTED_DATA = -802;
  SQLE_FAILED_TO_ENLIST                 = -803;
  SQLE_FAILED_TO_REENLIST               = -804;
  SQLE_SYNC_OPTION_NOT_FOUND            = -805;
  SQLE_EVENT_TYPE_NOT_FOUND             = -806;
  SQLE_HOSTVARS_IN_BATCH                = -807;
  SQLE_TEST_HARNESS                     = -808;
  SQLE_SYNC_OPTION_TOO_LONG             = -809;
  SQLE_JAVA_CLASS_NOT_FOUND             = -810;
  SQLE_JAR_NOT_FOUND                    = -811;
  SQLE_NO_MATCHING_SELECT_ITEM          = -812;
  SQLE_NON_UPDATEABLE_CURSOR            = -813;
  SQLE_CANNOT_UPDATE_ORDER_BY_COLUMN    = -814;
  SQLE_UPDATE_NOT_IN_SELECT             = -815;
  SQLE_FILE_IN_USE                      = -816;
  SQLE_TOO_MANY_TEMP_TABLES             = -817;
  SQLE_INCOMPATIBLE_EXTERNAL_CALL       = -818;
  SQLE_SYNC_CONTAINS_TABLE              = -819;
  SQLE_INVALID_POSITION                 = -820;
  SQLE_DBSPACE_FOR_TABLE_UNAVAILABLE    = -821;
  SQLE_TABLE_ALREADY_INCLUDED           = -822;
  SQLE_OMNI_EXPRESSION_IN_PROC          = -823;
  SQLE_INVALID_CORRELATION_NAME_REFERENCE = -824;
  SQLE_PREVIOUS_ERROR_LOST              = -825;
  SQLE_DBO_DOES_NOT_OWN_ROWGENERATOR    = -826;
  SQLE_USER_TABLES_IN_SYSTEM_RANGE      = -827;
  SQLE_RECORDING_CONFLICT_DETECTED      = -828;
  SQLE_CERTICOM_HANDSHAKE_FAILED        = -829;
  SQLE_ALIAS_NOT_UNIQUE                 = -830;
  SQLE_ALIAS_NOT_YET_DEFINED            = -831;
  SQLE_CONNECTION_ERROR                 = -832;
  SQLE_ROW_REMODIFIED_OR_DELETED        = -833;
  SQLE_COLUMN_NOT_FOUND_IN_TABLE        = -834;
  SQLE_EVENT_IN_USE                     = -835;
  SQLE_PROCEDURE_NO_LONGER_VALID        = -836;
  SQLE_TRIGGER_NO_LONGER_VALID          = -837;
  SQLE_CERTICOM_INIT_FAILED_ON_SRV      = -838;
  SQLE_DOWNLOAD_CONFLICT                = -839;
  SQLE_BAD_ENCRYPTION_KEY               = -840;
  SQLE_JDK_VERSION_MISMATCH             = -841;
  SQLE_UNSUPPORTED_JDK                  = -842;
  SQLE_SYNC_SITE_NOT_FOUND              = -843;
  SQLE_SYNC_DEFINITION_NOT_FOUND        = -844;
  SQLE_INVALID_COLUMN_QUALIFICATION     = -845;
  SQLE_INVALID_SET_CLAUSE               = -846;
  SQLE_JAVA_SECMGR_NOT_FOUND            = -847;
  SQLE_JAVA_SECMGR_EXCEPTION            = -848;
  SQLE_INVALID_FOREIGN_KEY_ACTION       = -849;
  SQLE_INVALID_FOREIGN_KEY_TABLE        = -850;
  SQLE_DECRYPT_ERROR                    = -851;
  SQLE_AMBIGUOUS_TABLE_NAME             = -852;
  SQLE_CURSOR_INVALID_STATE             = -853;
  SQLE_INVALID_ORDERBY_COLUMN           = -854;
  SQLE_AMBIGUOUS_TRIGGER_NAME           = -855;
  SQLE_INVALID_SQLLEN                   = -856;
  SQLE_SERVER_SYNCHRONIZATION_ERROR     = -857;
  SQLE_HISTOGRAMS_NOT_SUPPORTED_ON_OBJECT = -858;
  SQLE_JAVA_IN_USE                      = -859;
  SQLE_DBSPACE_NOT_CALIBRATED           = -860;
  SQLE_MULTIPLE_AGGREGATE_COLUMNS       = -861;
  SQLE_INVALID_AGGREGATE_PLACEMENT      = -862;
  SQLE_INVALID_DISTINCT_AGGREGATE       = -863;
  SQLE_INVALID_NUMBER                   = -864;
  SQLE_INVALID_FOREIGN_KEY_INDEX        = -865;
  SQLE_CANNOT_CHANGE_USER_NAME          = -867;
  SQLE_UNKNOWN_CHARSET                  = -868;
  SQLE_UNSUPPORTED_CHARACTER_SET_ERROR  = -869;
  SQLE_FAILED_TO_START_CONVERTER        = -870;
  SQLE_JAVA_DB_RESTART_NEEDED           = -871;
  SQLE_CANNOT_JOIN_TABEXPR              = -875;
  SQLE_CLIENT_OUT_OF_MEMORY             = -876;
  SQLE_SETUSER_NOT_IN_PROCEDURES        = -877;
  SQLE_CLUSTERED_INDEX_NOT_ALLOWED      = -878;
  SQLE_INPUT_ILLEGAL_MULTIBYTE_ERROR    = -879;
  SQLE_OUTPUT_ILLEGAL_MULTIBYTE_ERROR   = -880;
  SQLE_INPUT_SIMPLE_SUBSTITUTION_ERROR  = -881;
  SQLE_OUTPUT_SIMPLE_SUBSTITUTION_ERROR = -882;
  SQLE_CHARACTER_CONVERSION_REPORT_NOT_AVAILABLE = -883;
  SQLE_NONDETERMINISTIC_FUNCTION        = -889;
  SQLE_SYNTACTIC_LIMIT                  = -890;
  SQLE_FAILED_TO_START_CONVERTER_2_CHARSETS = -891;
  SQLE_STMT_NOT_ALLOWED_IN_PLAN         = -894;
  SQLE_ENCRYPT_ERROR                    = -895;
  SQLE_UNSET_PUBLIC_ID                  = -896;
  SQLE_BAD_FOR_XML_EXPLICIT_TAG         = -897;
  SQLE_BAD_FOR_XML_EXPLICIT_DIRECTIVE   = -898;
  SQLE_BAD_FOR_XML_EXPLICIT_COLUMN_NAME = -899;
  SQLE_EXPLICIT_TAG_NOT_OPEN            = -900;
  SQLE_UNDECLARED_FOR_XML_EXPLICIT_TAG  = -901;
  SQLE_FOR_XML_EXPLICIT_TOO_FEW_COLUMNS = -902;
  SQLE_FOR_XML_EXPLICIT_NAMED_CDATA     = -903;
  SQLE_INVALID_ORDERBY_IN_AGGREGATE     = -904;
  SQLE_INVALID_XQUERY_CONSTRUCTOR       = -905;
  SQLE_XMLGEN_EVALUATION_FAILURE        = -906;
  SQLE_NO_SQLX_ARGUMENT_NAME            = -907;
  SQLE_ULTRALITE_OBJ_CLOSED             = -908;
  SQLE_SYSTEM_COMMAND_FAILED            = -910;
  SQLE_IQ_LOG_REQUIRED                  = -933;
  SQLE_UNABLE_TO_START_DATABASE_VER_NEWER = -934;
  SQLE_PASSWORD_TOO_LONG                = -958;
  SQLE_ILLEGAL_PASSWORD                 = -963;
  SQLE_ALREADY_HAS_EXEC_PERMS           = -968;
  SQLE_PAGE_SIZE_TOO_SMALL              = -972;
  SQLE_STRING_PARM_TOO_LONG             = -973;
  SQLE_INVALID_TEMP_TABLE_COMMIT_ACTION = -993;
  SQLE_TOO_MANY_ARGUMENTS               = -994;
  SQLE_TEMP_SPACE_LIMIT                 = -1000;
  SQLE_FILE_NOT_DB                      = -1004;
  SQLE_FILE_WRONG_VERSION               = -1005;
  SQLE_FILE_BAD_DB                      = -1006;
  SQLE_CORRUPT_REDO                     = -1007;
  SQLE_CORRUPT_REDO_MIRROR              = -1008;
  SQLE_CORRUPT_REDO_OR_MIRROR           = -1009;
  SQLE_EXPECTING_NO_REDO                = -1010;
  SQLE_UNKNOWN_ENCRYPTION_ALGORITHM     = -1011;
  SQLE_UPGRADE_DATABASE                 = -1012;
  SQLE_NO_DB_FOR_WRITE                  = -1013;
  SQLE_FILE_IS_COMPRESSED               = -1014;
  SQLE_OLD_WRITE                        = -1015;
  SQLE_LOG_NEWER_THAN_DB                = -1016;
  SQLE_LOG_NOT_FOUND                    = -1017;
  SQLE_LOG_OFFSETS_DONT_MATCH           = -1018;
  SQLE_LOG_OLDER_THAN_DB                = -1019;
  SQLE_LOG_TOO_SHORT                    = -1020;

  {dpp_fetch}
  CUR_RELATIVE                  = 2;
  CUR_ABSOLUTE                  = 1;

  CUR_FORUPDATE                 = 8;
  CUR_FORREGULAR                = 0;

  CUR_DONTHOLD                  = 1;
  CUR_READONLY                  = 2;
  CUR_OPEN_DECLARE              = 32;
  CUR_DYNAMIC_SCROLL            = 64;
  CUR_SCROLL                    = 128;
  CUR_SENSITIVE                 = 256;
  CUR_INSENSITIVE               = 1024;
  CUR_UNIQUE                    = 2048;
  CUR_UPDATE                    = 4096;
  CUR_UPDATEBYLOCK              = CUR_UPDATE + 512;
  CUR_UPDATEBYVALUES            = CUR_UPDATEBYLOCK + 8192;
  CUR_UPDATEBYTIMESTAMP         = CUR_UPDATE + 8192;
  CUR_NOSCROLL                  = 16384;

  SQL_DESCRIBE_OUTPUT           = 1;
  SQL_DESCRIBE_INPUT            = 2;
  SQL_DESCRIBE_ALL              = 3;
  SQL_DESCRIBE_VAR_RESULT       = 8;

  SQL_PREPARE_DESCRIBE_STMTNAME = 1;
  SQL_PREPARE_DESCRIBE_STMTNUM  = 3;
  SQL_PREPARE_DESCRIBE_OUTPUT   = 256;
  SQL_PREPARE_DESCRIBE_INPUT    = 512;
  SQL_PREPARE_DESCRIBE_LONGNAMES= 1024;
  SQL_PREPARE_DESCRIBE_VARRESULT= 2048;

  SQL_LONGNAMES_COLUMN          = 1;
  SQL_LONGNAMES_TABLE           = 3;
  SQL_LONGNAMES_OWNER           = 7;


  DT_PROCEDURE_OUT    =	$8000;
  DT_PROCEDURE_IN     =	$4000;
  DT_UPDATEABLE       = $2000;
  DT_DESCRIBE_INPUT   = $1000;
  DT_AUTO_INCREMENT   = $0800;
  DT_KEY_COLUMN		    = $0400;
  DT_HIDDEN_COLUMN	  = $0200;
  DT_HAS_USERTYPE_INFO = $0100;

  DT_NULLS_ALLOWED  = $0001;

{dataTypes }
  DT_NOTYPE             =   0;
  DT_SMALLINT           = 500;
  DT_INT                = 496;
  DT_DECIMAL            = 484;
  DT_FLOAT              = 482;
  DT_DOUBLE             = 480;
  DT_DATE               = 384;
  DT_STRING             = 460;
  DT_FIXCHAR            = 452;
  DT_VARCHAR            = 448;
  DT_LONGVARCHAR        = 456;
  DT_TIME               = 388;
  DT_TIMESTAMP          = 392;
  DT_TIMESTAMP_STRUCT   = 390;
  DT_BINARY             = 524;
  DT_LONGBINARY         = 528;
  DT_VARIABLE           = 600;
  DT_TINYINT            = 604;
  DT_BIGINT             = 608;
  DT_UNSINT             = 612;
  DT_UNSSMALLINT        = 616;
  DT_UNSBIGINT          = 620;
  DT_BIT                = 624;
  DT_NSTRING            = 628;
  DT_NFIXCHAR           = 632;
  DT_NVARCHAR           = 636;
  DT_LONGNVARCHAR       = 640;

//Message Types Markus
   MESSAGE_TYPE_INFO	   = 0;
   MESSAGE_TYPE_WARNING	 = 1;
   MESSAGE_TYPE_ACTION	 = 2;
   MESSAGE_TYPE_STATUS   = 3;

type
  a_sql_len = SmallInt;
  a_sql_ulen = Word;
  a_sql_int32 = LongInt;
  a_sql_type = SmallInt;

  an_sql_code = LongInt;
  an_sql_state = array[0..5] of AnsiChar;

type
  TZASASQLWARN = record
      sqlWarn0: array[0..0] of AnsiChar;
      sqlWarn1: array[0..0] of AnsiChar;
      sqlWarn2: array[0..0] of AnsiChar;
      sqlWarn3: array[0..0] of AnsiChar;
      sqlWarn4: array[0..0] of AnsiChar;
      sqlWarn5: array[0..0] of AnsiChar;
      sqlWarn6: array[0..0] of AnsiChar;
      sqlWarn7: array[0..0] of AnsiChar;
      sqlWarn8: array[0..0] of AnsiChar;
      sqlWarn9: array[0..0] of AnsiChar;
  end;

  PPZASASQLCA = ^PZASASQLCA;
  PZASASQLCA = ^TZASASQLCA;
  TZASASQLCA = record
    sqlcaID: array[0..7] of AnsiChar;
    sqlcAbc   : a_sql_int32;
    sqlCode   : an_sql_code;
    sqlErrml  : SmallInt;
    sqlErrmc  : array[0..69] of AnsiChar;
    sqlErrp   : array[0..7] of AnsiChar;
    sqlErrd   : array[0..5] of a_sql_int32;
    sqlWarn   : TZASASQLWARN;
    sqlState: array[0..5] of AnsiChar;
  end;

  PZASASQLNAME = ^TZASASQLNAME;
  TZASASQLNAME = record
    length : Word;
    data: array[0..29] of AnsiChar;
  end;

  PZASASQLSTRING = ^TZASASQLSTRING;
  TZASASQLSTRING = record
    length : Word;
    data: array[0..0] of AnsiChar;
  end;

  PZASASQLVAR = ^TZASASQLVAR;
  TZASASQLVAR = record
    sqlType: SmallInt;
    sqlLen : a_sql_len;
    sqlData: Pointer;
    sqlInd : PSmallInt;
    sqlName: TZASASQLNAME;
  end;

  PASASQLDA = ^TASASQLDA;
  TASASQLDA = record
    sqldaid: array[0..7] of AnsiChar;
    sqldabc : LongWord;
    sqln    : SmallInt;
    sqld    : SmallInt;
    sqlVar  : array[0..32767] of TZASASQLVAR;
  end;

  PZASASQLDateTime = ^TZASASQLDateTime;
  TZASASQLDateTime = packed record
    Year             : SmallInt;  //* e.g. 1992
    Month            : Byte;      //* 0-11
    Day_of_Week      : Byte;      //* 0-6  0=Sunday, 1=Monday, ...
    Day_of_Year      : SmallInt;  //* 0-365
    Day              : Byte;      //* 1-31
    Hour             : Byte;      //* 0-23
    Minute           : Byte;      //* 0-59
    Second           : Byte;      //* 0-59
    MicroSecond      : LongInt;   //* 0-999999
  end;

  PZASABlobStruct = ^TZASABlobStruct;
  TZASABlobStruct = record
    array_len        : Longword;
    stored_len       : Longword;
    untrunc_len      : Longword;
    arr: array[0..0] of AnsiChar;
  end;

  ZASA_db_callback_index = (             //Markus
    DB_CALLBACK_START,
    DB_CALLBACK_WAIT,
    DB_CALLBACK_FINISH,
    DB_CALLBACK_ASYNC_RESPONSE,
    DB_CALLBACK_TRANSLATE_IN,
    DB_CALLBACK_TRANSLATE_OUT,
    DB_CALLBACK_UNKNOWN_EVENT,	    // For Macintosh, initially
    DB_CALLBACK_MESSAGE,	    // Messages from the engine during a request
    DB_CALLBACK_CONN_DROPPED,
    DB_CALLBACK_DEBUG_MESSAGE,
    DB_MAX_CALLBACK
    );

  TZASASQLCallback = procedure() {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  {** Represents a generic interface to ASA native API. }
  IZASAPlainDriver = interface (IZPlainDriver)
    ['{86AFDDD6-D401-4A30-B3BE-4AC5095E13F0}']
  end;

  {** Implements a driver for ASA 7.0-9.0}
  TZASAPlainDriver = class (TZAbstractPlainDriver, IZASAPlainDriver)
  public
    db_init: function( sqlca: PZASASQLCA): Integer;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    db_fini: function( sqlca: PZASASQLCA): Integer;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    db_string_connect: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    db_string_disconnect: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    db_start_engine: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    db_stop_engine: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    db_start_database: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    db_stop_database: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    db_find_engine: function(sqlca: PZASASQLCA; Params: PAnsiChar): Word;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Alloc_sqlda: function( NumVar: LongWord): PASASQLDA;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    fill_sqlda: function( Parameter: PASASQLDA): PASASQLDA;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    free_filled_sqlda: procedure( Parameter: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    fill_s_sqlda: function( Parameter: PASASQLDA; MaxLength: Integer): PASASQLDA;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    free_sqlda: procedure( Parameter: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    free_sqlda_noind: procedure( Parameter: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_setConnect: procedure(sqlca: PZASASQLCA; ConnName: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_disconnect: procedure(sqlca: PZASASQLCA; ConnName: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_describe_cursor: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; SomeNumber: LongWord);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_prepare_into: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; SomeNumber: LongWord);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_prepare_describe: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord;
      LongNames: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    {ASA12 dbpp_prepare_describe_12, (SQLCA *,char *,char *,short int *,char *,struct sqlda *,struct sqlda *,unsigned int, unsigned short int, a_sql_uint32 ))}
    dbpp_prepare_describe_12: procedure (SQLCA: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord;
      LongNames: Word; UnknownUint2: Longword)
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    {ASA16 dbpp_prepare_describe_16, SQLCA *,char *,char *,short int *,char *,struct sqlda *,struct sqlda *,unsigned int, unsigned short int, a_sql_uint32 )))}
    dbpp_prepare_describe_16: procedure (SQLCA: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord;
      LongNames: Word; UnknownUint2: Longword)
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_select: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; Descriptor1,
      Descriptor2: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_open: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      UnKnown: PAnsiChar; ProgName: PAnsiChar; RecordStatementNum: PSmallInt;
      Descriptor1: PASASQLDA; BlockSize: SmallInt; IsolationLvl: SmallInt;
      Options : Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_close: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_fetch: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Offset: Word; RelPositon: LongInt; Descriptor1: PASASQLDA;
      BlockSize: SmallInt; Options: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_declare: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      UnKnown: PAnsiChar; ProgName: PAnsiChar; RecordStatementNum: PSmallInt;
      Options: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_dropstmt: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_describe: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; Descriptor: PASASQLDA;
      WhatToDesc: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_delete: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      UnKnown1: PAnsiChar; UnKnown2: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_update: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_execute_imm: procedure(sqlca: PZASASQLCA; SqlRecordStatement:
      PAnsiChar; UnKnown1: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_put_into: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; UnKnown1: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_put_array: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; Into_sqlda: PASASQLDA; Rows: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_commit: procedure( sqlca: PZASASQLCA; SomeNumber: LongWord);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_rollback: procedure( sqlca: PZASASQLCA; SomeNumber: LongWord);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_execute_into: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; Descriptor1: PASASQLDA;
      Descriptor2: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_get_data: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      ColumnNumber: Word; Offset: LongInt; Descriptor1: PASASQLDA;
      Unknown: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_explain: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      SomeNumber1: Word; Descriptor1: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    db_register_a_callback: procedure( sqlca: PZASASQLCA;
      CBIdx: integer; Proc: TZASASQLCallback);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_setoption: procedure( sqlca: PZASASQLCA; Temporary: LongInt;
      User: PAnsiChar; Option: PAnsiChar; Descriptor: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_fetch_array: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Offset: Word; RelPositon: LongInt; Descriptor1: PASASQLDA;
      BlockSize: SmallInt; Options: Word; ArrayWidth: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    sqlerror_message: function(sqlca: PZASASQLCA; Buffer: PAnsiChar;
      MaxSize: Integer): PAnsiChar;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    dbpp_resume: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    db_cancel_request: function( sqlca: PZASASQLCA): Integer;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    db_change_char_charset: function( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    db_change_nchar_charset: function( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  protected
    procedure LoadApi; override;
    function GetUnicodeCodePageName: String; override;
  public
    procedure LoadCodePages; override;
    constructor Create;

    function Clone: IZPlainDriver; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

{$ENDIF ZEOS_DISABLE_ASA}

implementation

{$IFNDEF ZEOS_DISABLE_ASA}

uses SysUtils, ZPlainLoader, ZEncoding;

procedure TZASAPlainDriver.LoadApi;
begin
  with FLoader do
  begin
    @sqlerror_message       := GetAddress('sqlerror_message', True);
    @db_init                := GetAddress('db_init', True);
    @db_fini                := GetAddress('db_fini');
    @db_string_connect      := GetAddress('db_string_connect');
    @db_string_disconnect   := GetAddress('db_string_disconnect');
    @db_find_engine         := GetAddress('db_find_engine');
    @db_start_engine        := GetAddress('db_start_engine');
    @db_stop_engine         := GetAddress('db_stop_engine');
    @db_start_database      := GetAddress('db_start_database');
    @db_stop_database       := GetAddress('db_stop_database');
    @alloc_sqlda            := GetAddress('alloc_sqlda');
    @fill_sqlda             := GetAddress('fill_sqlda');
    @fill_s_sqlda           := GetAddress('fill_s_sqlda');
    @free_filled_sqlda      := GetAddress('free_filled_sqlda');
    @free_sqlda             := GetAddress('free_sqlda');
    @free_sqlda_noind       := GetAddress('free_sqlda_noind');
    @dbpp_setConnect        := GetAddress('dbpp_setconnect');
    @dbpp_disconnect        := GetAddress('dbpp_disconnect');
    @dbpp_prepare_into      := GetAddress('dbpp_prepare_into');
    @dbpp_describe_cursor   := GetAddress('dbpp_describe_cursor');
    @dbpp_prepare_describe_16  := GetAddress('dbpp_prepare_describe_16');
    @dbpp_prepare_describe_12  := GetAddress('dbpp_prepare_describe_12');
    @dbpp_prepare_describe  := GetAddress('dbpp_prepare_describe', not Assigned(dbpp_prepare_describe_16) and not Assigned(dbpp_prepare_describe_12));
    @dbpp_select            := GetAddress('dbpp_select');
    @dbpp_open              := GetAddress('dbpp_open', True);
    @dbpp_close             := GetAddress('dbpp_close');
    @dbpp_fetch             := GetAddress('dbpp_fetch');
    @dbpp_declare           := GetAddress('dbpp_declare');
    @dbpp_dropstmt          := GetAddress('dbpp_dropstmt');
    @dbpp_describe          := GetAddress('dbpp_describe');
    @dbpp_delete            := GetAddress('dbpp_delete');
    @dbpp_update            := GetAddress('dbpp_update');
    @dbpp_put_into          := GetAddress('dbpp_put_into');
    @dbpp_put_array         := GetAddress('dbpp_put_array');
    @dbpp_execute_imm       := GetAddress('dbpp_execute_imm');
    @dbpp_commit            := GetAddress('dbpp_commit');
    @dbpp_rollback          := GetAddress('dbpp_rollback');
    @dbpp_execute_into      := GetAddress('dbpp_execute_into');
    @dbpp_get_data          := GetAddress('dbpp_get_data');
    @dbpp_explain           := GetAddress('dbpp_explain');
    @dbpp_setoption         := GetAddress('dbpp_setoption');
    @dbpp_fetch_array       := GetAddress('dbpp_fetch_array');
    @db_register_a_callback := GetAddress('db_register_a_callback');
    @dbpp_resume            := GetAddress('dbpp_resume');
    @db_cancel_request      := GetAddress('db_cancel_request');
    @db_change_char_charset := GetAddress('db_change_char_charset');
    @db_change_nchar_charset:= GetAddress('db_change_nchar_charset');
  end;
end;

function TZASAPlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Sybase ASA';
end;

function TZASAPlainDriver.GetProtocol: string;
begin
  Result := 'ASA';
end;

function TZASAPlainDriver.GetUnicodeCodePageName: String;
begin
  Result := 'UTF-8';
end;

procedure TZASAPlainDriver.LoadCodePages;
begin
  { MultiByte }
  AddCodePage('TIS-620', 1, ceAnsi, 874); {Windows Thail�ndisch, ISO8859-11, bin�re Sortierung}
  AddCodePage('Windows-31J', 2, ceAnsi, 932); {Japanese Shift-JIS mit Microsoft-Erweiterungen}
  AddCodePage('GBK', 3, ceAnsi, 936); {GB2312-80 Simplified Chinese}
  AddCodePage('IBM949', 4, ceAnsi, 949); {Korean KS C 5601-1987-Codierung, Wansung}
  AddCodePage('BIG5', 5, ceAnsi, 950); {Traditionelles Chinesisch, Big 5-Kodierung mit HKSCS}
  AddCodePage('EUC_CHINA', 6, ceAnsi, zCP_GB2312); {GB2312-80 Simplified Chinese}
  AddCodePage('UTF-8', 7, ceUTF8, zCP_UTF8, '', 3); {UTF-8, 8-Bit-Mehrbyte-Zeichensatz f�r Unicode, bin�re Reihenfolge}

  { SingleByte }
  AddCodePage('Windows-1250', 8, ceAnsi, 1250); {Windows Latin 2, Polnisch}
  AddCodePage('Windows-1251', 9, ceAnsi, 1251); {Windows Kyrillisch}
  AddCodePage('Windows-1252', 10, ceAnsi, 1252); { Windows Latin 1, Western}
  AddCodePage('Windows-1253', 11, ceAnsi, 1253); {Windows Griechisch, ISO8859-7 mit Erweiterungen}
  AddCodePage('Windows-1254', 12, ceAnsi, 1254); {Windows T�rkisch, ISO8859-9 mit Erweiterungen}
  AddCodePage('Windows-1255', 13, ceAnsi, 1255); {Windows Hebr�isch, ISO8859-8 mit Erweiterungen}
  AddCodePage('Windows-1256', 14, ceAnsi, 1256); {Windows Arabisch, ISO8859-6 mit Erweiterungen}
  AddCodePage('Windows-1257', 15, ceAnsi, 1257); {Windows Baltische Staaten, Litauisch}
  AddCodePage('Windows-1258', 16, ceAnsi, 1258); {Windows }

  {*nix}
  AddCodePage('ISO_8859-6:1987', 17, ceAnsi, 1256); {Arabisch, ISO8859-6 mit Erweiterungen}
  AddCodePage('ISO_8859-2:1987', 18, ceAnsi, 1251); {Zentral- und Osteurop�isch}
  //ISO-8859-15 //ISO9LATIN1
  //ISO_8859-7:1987 //Griechisch
  //ISO_8859-8:1988 //Hebr�isch
  //ISO-8859-15 //Italienisch
  //EUC-JP //Japanisch
  //EUC-KR //Koreanisch
  //ISO_8859-5:1988 //Russisch
  AddCodePage('GB2312', 19, ceAnsi, zCP_GB2312); {GB2312-80 Simplified Chinese}
  //EUC-TW //Traditionelles Chinesisch - Taiwan
  AddCodePage('Big5-HKSCS', 20, ceAnsi, 950); {Traditionelles Chinesisch, Big 5-Kodierung mit HKSCS}
  AddCodePage('ISO_8859-9:1989', 21, ceAnsi, 920); //T�rkisch
end;

function TZASAPlainDriver.Clone: IZPlainDriver;
begin
  Result := TZASAPlainDriver.Create;
end;

constructor TZASAPlainDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
  FLoader.AddLocation({$IFNDEF LINUX}ASA7_WINDOWS_DLL_LOCATION{$ELSE}ASA7_LINUX_DLL_LOCATION{$ENDIF});
  FLoader.AddLocation({$IFNDEF LINUX}ASA8_WINDOWS_DLL_LOCATION{$ELSE}ASA8_LINUX_DLL_LOCATION{$ENDIF});
  FLoader.AddLocation({$IFNDEF LINUX}ASA9_WINDOWS_DLL_LOCATION{$ELSE}ASA9_LINUX_DLL_LOCATION{$ENDIF});
  FLoader.AddLocation({$IFNDEF LINUX}ASA12_WINDOWS_DLL_LOCATION{$ELSE}ASA12_LINUX_DLL_LOCATION{$ENDIF});
  FLoader.AddLocation({$IFNDEF LINUX}ASA17_WINDOWS_DLL_LOCATION{$ELSE}ASA17_LINUX_DLL_LOCATION{$ENDIF});
  LoadCodePages;
end;

{$ENDIF ZEOS_DISABLE_ASA}

end.

