{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Native Plain Drivers for SQL Anywhere CAPI       }
{                                                         }
{            Originally written by EgonHugeist            }
{                                                         }
{*********************************************************}

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

unit ZPlainSQLAnywhere;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_SQLANY}

uses Classes, ZCompatibility, ZPlainDriver;

const
  SQLAnyLibrary = {$IFDEF MSWINDOWS}'dbcapi'{$ELSE}'libdbcapi'{$ENDIF}+SharedSuffix;

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

  /// <summary>
  ///  Version 1 was the initial version of the C/C++ API.
  /// </summary>
  SQLANY_API_VERSION_1 = 1;
  /// <summary>
  ///  Version 2 introduced the "_ex" functions and the ability to cancel requests.
  /// </summary>
  SQLANY_API_VERSION_2 = 2;
  /// <summary>
  ///  Version 3 introduced the "callback" function.
  /// </summary>
  SQLANY_API_VERSION_3 = 3;
  /// <summary>
  ///  Version 4 introduced NCHAR support and wide inserts.
  /// </summary>
  SQLANY_API_VERSION_4 = 4;
  /// <summary>
  ///  Version 5 introduced a way to reset sent data through sqlany_send_param_data() * and the A_FLOAT data type
  /// </summary>
  SQLANY_API_VERSION_5 = 5;

  SACAPI_VERSION = SQLANY_API_VERSION_5; //switch to latest version

  /// <summary>
  ///  Returns the minimal error buffer size.
  /// </summary>
  SACAPI_ERROR_SIZE = 256;

type
  /// <summary>
  ///  A handle to an interface context
  /// </summary>
  Pa_sqlany_interface_context = ^Ta_sqlany_interface_context;
  Ta_sqlany_interface_context = record end;

  /// <summary>
  ///  A handle to a connection object
  /// </summary>
  Pa_sqlany_connection = ^Ta_sqlany_connection;
  Ta_sqlany_connection = record end;

  /// <summary>
  ///  An address of address to a handle of a statement object
  /// </summary>
  PPa_sqlany_stmt = ^Pa_sqlany_stmt;
  /// <summary>
  ///  An address to a handle of a statement object
  /// </summary>
  Pa_sqlany_stmt = ^Ta_sqlany_stmt;
  /// <summary>
  ///  A handle to a statement object
  /// </summary>
  Ta_sqlany_stmt = record end;

  /// <summary>
  ///  A portable 32-bit signed value
  /// </summary>
  Tsacapi_i32 = type Integer;
  /// <summary>
  ///  A pointer to a portable 32-bit signed value
  /// </summary>
  Psacapi_i32 = ^Tsacapi_i32;
  /// <summary>
  ///  A portable 32-bit unsigned value
  /// </summary>
  Tsacapi_u32 = type Cardinal;
  /// <summary>
  ///  A pointer to a portable 32-bit unsigned value
  /// </summary>
  Psacapi_u32 = ^Tsacapi_u32;
  /// <summary>
  ///  A portable boolean value
  /// </summary>
  Tsacapi_bool = type Tsacapi_i32;

  Psacapi_i32Array = ^Tsacapi_i32Array;
  Tsacapi_i32Array = array[Byte] of Tsacapi_i32;

  TSQLANY_CALLBACK = procedure() {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};

{$Z+} //delphi byte enum size to dword size
  /// <summary>
  ///  Specifies the data type being passed in or retrieved.
  /// </summary>
  Ta_sqlany_data_type = (
    /// <summary>
    ///  Invalid data type.
    /// </summary>
    A_INVALID_TYPE,
    /// <summary>
    /// Binary data.  Binary data is treated as-is and no character set conversion is performed.
    /// </summary>
    A_BINARY,
    /// <summary>
    /// String data.  The data where character set conversion is performed.
    /// </summary>
    A_STRING,
    /// <summary>
    /// Double data.  Includes float values.
    /// </summary>
    A_DOUBLE,
    /// <summary>
    /// 64-bit signed integer
    /// </summary>
    A_VAL64,
    /// <summary>
    /// 64-bit unsigned integer
    /// </summary>
    A_UVAL64,
    /// <summary>
    /// 32-bit signed integer
    /// </summary>
    A_VAL32,
    /// <summary>
    /// 32-bit unsigned integer
    /// </summary>
    A_UVAL32,
    /// <summary>
    /// 16-bit signed integer
    /// </summary>
    A_VAL16,
    /// <summary>
    /// 16-bit unsigned integer
    /// </summary>
    A_UVAL16,
    /// <summary>
    /// 8-bit signed integer
    /// </summary>
    A_VAL8,
    /// <summary>
    /// 8-bit unsigned integer
    /// </summary>
    A_UVAL8,
    /// <summary>
    /// Float precision data. available since version 5
    /// </summary>
    A_FLOAT
  );

  PSize_t = ^Tsize_t;
  Tsize_t = NativeUint;

  Psize_tArray = ^Tsize_tArray;
  Tsize_tArray = Array[Byte] of Tsize_t;
  /// <summary>
  /// Returns a description of the attributes of a data value.
  /// </summary>
  Ta_sqlany_data_value = record
    /// <summary>
    /// A pointer to user supplied buffer of data.
    /// </summary>
    buffer: PAnsiChar;
    /// <summary>
    /// The size of the buffer.
    /// </summary>
    buffer_size: Tsize_t;
    /// <summary>
    /// A pointer to the number of valid bytes in the buffer. This value must be less than buffer_size.
    /// </summary>
    length: Psize_t;
    /// <summary>
    /// The type of the data.
    /// </summary>
    _type: Ta_sqlany_data_type;
    /// <summary>
    /// A pointer to indicate whether the last fetched data is NULL.
    /// </summary>
    is_null: Psacapi_i32;
  end;
  Pa_sqlany_data_value = ^Ta_sqlany_data_value;

  Pa_sqlany_data_valueArray = ^Ta_sqlany_data_valueArray;
  Ta_sqlany_data_valueArray = array[Byte] of Ta_sqlany_data_value;

  /// <summary>
  /// Returns a description of the attributes of a data value. min version is V4
  /// </summary>
  Ta_sqlany_data_valueV4up = record
    /// <summary>
    /// A pointer to user supplied buffer of data.
    /// </summary>
    buffer: PAnsiChar;
    /// <summary>
    /// The size of the buffer.
    /// </summary>
    buffer_size: Tsize_t;
    /// <summary>
    /// A pointer to the number of valid bytes in the buffer. This value must be less than buffer_size.
    /// </summary>
    length: Psize_tArray;
    /// <summary>
    /// The type of the data.
    /// </summary>
    _type: Ta_sqlany_data_type;
    /// <summary>
    /// A pointer to indicate whether the last fetched data is NULL.
    /// </summary>
    is_null: Psacapi_i32Array;
    /// <summary>
    /// Indicates whether the buffer value is an pointer to the actual value.
    /// </summary>
    is_address: Tsacapi_i32;
  end;

  Pa_sqlany_data_valueV4up = ^Ta_sqlany_data_valueV4up;

  Pa_sqlany_data_valueV4upArray = ^Ta_sqlany_data_valueV4upArray;
  Ta_sqlany_data_valueV4upArray = array[Byte] of Ta_sqlany_data_valueV4up;

  /// <summary>
  ///  A data direction enumeration.
  /// </summary>
  Ta_sqlany_data_direction = (
    /// <summary>
    ///  Invalid data direction.
    /// </summary>
    DD_INVALID = $0000,
    /// <summary>
    /// Input-only host variables.
    /// </summary>
    DD_INPUT = $0001,
    /// <summary>
    /// Output-only host variables.
    /// </summary>
    DD_OUTPUT = $0002,
    /// <summary>
    /// Input and output host variables.
    /// </summary>
    DD_INPUT_OUTPUT = $0003
  );

  /// <summary>
  /// A bind parameter structure used to bind parameter and prepared statements.
  /// </summary>
  Ta_sqlany_bind_param = record
    /// <summary>
    /// The direction of the data. (input, output, input_output)
    /// </summary>
    direction: Ta_sqlany_data_direction;
    /// <summary>
    /// The actual value of the data.
    /// </summary>
    value: Ta_sqlany_data_value;
    /// <summary>
    /// Name of the bind parameter. This is only used by sqlany_describe_bind_param()
    /// </summary>
    name: PAnsiChar;
  end;
  /// <summary>
  /// A pointer to a bind parameter structure used to bind parameter and prepared statements.
  /// </summary>
  Pa_sqlany_bind_param = ^Ta_sqlany_bind_param;

  Pa_sqlany_bind_paramArray = ^Ta_sqlany_bind_paramArray;
  Ta_sqlany_bind_paramArray = array[Byte] of Ta_sqlany_bind_param;

  /// <summary>
  /// A bind parameter structure used to bind parameter and prepared statements.
  /// </summary>
  Ta_sqlany_bind_paramV4Up = record
    /// <summary>
    /// The direction of the data. (input, output, input_output)
    /// </summary>
    direction: Ta_sqlany_data_direction;
    /// <summary>
    /// The actual value of the data.
    /// </summary>
    value: Ta_sqlany_data_valueV4Up;
    /// <summary>
    /// Name of the bind parameter. This is only used by sqlany_describe_bind_param()
    /// </summary>
    name: PAnsiChar;
  end;
  /// <summary>
  /// A pointer to a bind parameter structure used to bind parameter and prepared statements.
  /// </summary>
  Pa_sqlany_bind_paramV4Up = ^Ta_sqlany_bind_paramV4Up;

  Pa_sqlany_bind_paramV4UpArray = ^Ta_sqlany_bind_paramV4UpArray;
  Ta_sqlany_bind_paramV4UpArray = array[Byte] of Ta_sqlany_bind_paramV4up;


  /// <summary>
  /// An enumeration of the native types of values as described by the server.
  /// The value types correspond to the embedded SQL data types.
  /// </summary>
  Ta_sqlany_native_type = (
    /// <summary>
    /// No data type.
    /// </summary>
    DT_NOTYPE = $0000,
    /// <summary>
    /// Null-terminated character string that is a valid date.
    /// </summary>
    DT_DATE = 384,
    /// <summary>
    /// Null-terminated character string that is a valid time.
    /// </summary>
    DT_TIME = 388,
    /// <summary>
    /// a timestamp struct type (not implemented in sacapi.h just known from our old ASA driver and the docs).
    ///  see: http://dcx.sybase.com/sa160/en/dbprogramming/pg-ruby-native-types.html
    /// </summary>
    DT_TIMESTAMP_STRUCT = 390,
    /// <summary>
    /// Null-terminated character string that is a valid timestamp.
    /// </summary>
    DT_TIMESTAMP = 392,
    /// <summary>
    ///  Varying length character string, in the CHAR character set, with a two-byte length field.
    ///  The maximum length is 32765 bytes. When sending data, you must set the length field.
    ///  When fetching data, the database server sets the length field.
    ///  The data is not null-terminated or blank-padded.
    /// </summary>
    DT_VARCHAR = 448,
    /// <summary>
    /// Fixed-length blank-padded character string, in the CHAR character set.
    /// The maximum length, specified in bytes, is 32767. The data is not null-terminated.
    /// </summary>
    DT_FIXCHAR = 452,
    /// <summary>
    /// Long varying length character string, in the CHAR character set.
    /// </summary>
    DT_LONGVARCHAR = 456,
    /// <summary>
    /// Null-terminated character string, in the CHAR character set.
    /// The string is blank-padded if the database is initialized with blank-padded strings.
    /// </summary>
    DT_STRING = 460,
    /// <summary>
    /// 8-byte floating-point number.
    /// </summary>
    DT_DOUBLE = 480,
    /// <summary>
    /// 4-byte floating-point number.
    /// </summary>
    DT_FLOAT = 482,
    /// <summary>
    /// Packed decimal number (proprietary format).
    /// </summary>
    DT_DECIMAL = 484,
    /// <summary>
    /// 32-bit signed integer.
    /// </summary>
    DT_INT = 496,
    /// <summary>
    /// 16-bit signed integer.
    /// </summary>
    DT_SMALLINT = 500,
    /// <summary>
    /// Varying length binary data with a two-byte length field.
    /// The maximum length is 32765 bytes. When supplying information to the database server,
    /// you must set the length field. When fetching information from the database server, the server sets the length field.
    /// </summary>
    DT_BINARY = 524,
    /// <summary>
    /// Long binary data.
    /// </summary>
    DT_LONGBINARY = 528,
    /// <summary>
    ///  a variable datatype (not implemented in sacapi.h just known from our old ASA driver and the docs).
    ///  see: http://dcx.sybase.com/sa160/en/dbprogramming/pg-ruby-native-types.html
    /// </summary>
    DT_VARIABLE = 600,
    /// <summary>
    /// 8-bit signed integer.
    /// </summary>
    DT_TINYINT = 604,
    /// <summary>
    /// 64-bit signed integer.
    /// </summary>
    DT_BIGINT = 608,
    /// <summary>
    /// 32-bit unsigned integer.
    /// </summary>
    DT_UNSINT = 612,
    /// <summary>
    /// 16-bit unsigned integer.
    /// </summary>
    DT_UNSSMALLINT = 616,
    /// <summary>
    /// 64-bit unsigned integer.
    /// </summary>
    DT_UNSBIGINT = 620,
    /// <summary>
    /// 8-bit signed integer.
    /// </summary>
    DT_BIT = 624,
    /// <summary>
    /// Null-terminated character string, in the NCHAR character set.
    /// The string is blank-padded if the database is initialized with blank-padded strings.
    /// </summary>
    DT_NSTRING = 628,
    /// <summary>
    /// Fixed-length blank-padded character string, in the NCHAR character set.
    /// The maximum length, specified in bytes, is 32767. The data is not null-terminated.
    /// </summary>
    DT_NFIXCHAR = 632,
    /// <summary>
    /// Varying length character string, in the NCHAR character set, with a two-byte length field.
    /// The maximum length is 32765 bytes. When sending data, you must set the length field.
    /// When fetching data, the database server sets the length field. The data is not null-terminated or blank-padded.
    /// </summary>
    DT_NVARCHAR = 636,
    /// <summary>
    /// Long varying length character string, in the NCHAR character set.
    /// </summary>
    DT_LONGNVARCHAR = 640
  );

  /// <summary>
  ///  Returns column metadata information.
  ///  sqlany_get_column_info() can be used to populate this structure.
  /// </summary>
  Ta_sqlany_column_info = record
    /// <summary>
    ///  The name of the column (null-terminated).
    ///  The string can be referenced as long as the result set object is not freed.
    /// </summary>
    name: PAnsiChar;
    /// <summary>
    ///  The column data type.
    /// </summary>
    _type: Ta_sqlany_data_type;
    /// <summary>
    ///  The native type of the column in the database.
    /// </summary>
    native_type: Ta_sqlany_native_type;
    /// <summary>
    ///  The precision.
    /// </summary>
    precision: word;
    /// <summary>
    ///  The scale.
    /// </summary>
    scale: word;
    /// <summary>
    ///  The maximum size a data value in this column can take.
    /// </summary>
    max_size: Tsize_t;
    /// <summary>
    ///  Indicates whether a value in the column can be null.
    /// </summary>
    nullable: Tsacapi_bool;
  end;
  /// <summary>
  ///  A pointer to the column metadata information.
  /// </summary>
  Pa_sqlany_column_info = ^Ta_sqlany_column_info;

  /// <summary>
  ///  Returns column metadata information. min Version is V4
  ///  sqlany_get_column_info() can be used to populate this structure.
  /// </summary>
  Ta_sqlany_column_infoV4up = record
    /// <summary>
    ///  The name of the column (null-terminated).
    ///  The string can be referenced as long as the result set object is not freed.
    /// </summary>
    name: PAnsiChar;
    /// <summary>
    ///  The column data type.
    /// </summary>
    _type: Ta_sqlany_data_type;
    /// <summary>
    ///  The native type of the column in the database.
    /// </summary>
    native_type: Ta_sqlany_native_type;
    /// <summary>
    ///  The precision.
    /// </summary>
    precision: word;
    /// <summary>
    ///  The scale.
    /// </summary>
    scale: word;
    /// <summary>
    ///  The maximum size a data value in this column can take.
    /// </summary>
    max_size: Tsize_t;
    /// <summary>
    ///  Indicates whether a value in the column can be null.
    /// </summary>
    nullable: Tsacapi_bool;
    /// <summary>
    ///  The name of the table (null-terminated).
    ///  The string can be referenced as long as the result set object is not freed.
    /// </summary>
    table_name: PAnsiChar;
    /// <summary>
    ///  The name of the owner (null-terminated).
    ///  The string can be referenced as long as the result set object is not freed.
    /// </summary>
    owner_name: PAnsiChar;
    /// <summary>
    ///  Indicates whether the column is bound to a user buffer.
    /// </summary>
    is_bound: Tsacapi_bool;
    /// <summary>
    ///  Information about the bound column.
    /// </summary>
    binding: Ta_sqlany_data_valueV4up;
  end;
  /// <summary>
  ///  A pointer to the column metadata information of version 4 and up.
  /// </summary>
  Pa_sqlany_column_infoV4up = ^Ta_sqlany_column_infoV4up;

  /// <summary>
  ///  Gets information about the currently bound parameters.
  ///  sqlany_get_bind_param_info() can be used to populate this structure.
  /// </summary>
  Ta_sqlany_bind_param_info = record
    /// <summary>
    ///  A pointer to the name of the parameter.
    /// </summary>
    name: PAnsiChar;
    /// <summary>
    ///  The direction of the parameter.
    /// </summary>
    direction: Ta_sqlany_data_direction;
    /// <summary>
    ///  Information about the bound input value.
    /// </summary>
    input_value: Ta_sqlany_data_value;
    /// <summary>
    ///  Information about the bound output value.
    /// </summary>
    output_value: Ta_sqlany_data_value;
  end;
  Pa_sqlany_bind_param_info = ^Ta_sqlany_bind_param_info;

  Ta_sqlany_bind_param_infoV4up = record
    /// <summary>
    ///  A pointer to the name of the parameter.
    /// </summary>
    name: PAnsiChar;
    /// <summary>
    ///  The direction of the parameter.
    /// </summary>
    direction: Ta_sqlany_data_direction;
    /// <summary>
    ///  Information about the bound input value.
    /// </summary>
    input_value: Ta_sqlany_data_value;
    /// <summary>
    ///  Information about the bound output value.
    /// </summary>
    output_value: Ta_sqlany_data_value;
    /// <summary>
    ///  The native type of the column in the database.
    /// </summary>
    native_type: Ta_sqlany_native_type;
    /// <summary>
    ///  The precision.
    /// </summary>
    precision: word;
    /// <summary>
    ///  The scale.
    /// </summary>
    scale: word;
    /// <summary>
    ///  The maximum size a data value in this column can take.
    /// </summary>
    max_size: Tsize_t;
  end;
  Pa_sqlany_bind_param_infoV4up = ^Ta_sqlany_bind_param_infoV4up;

  /// <summary>
  ///  Returns metadata information about a column value in a result set.
  ///  sqlany_get_data_info() can be used to populate this structure with
  ///  information about what was last retrieved by a fetch operation.
  /// </summary>
  Ta_sqlany_data_info = record
    /// <summary>
    ///  The type of the data in the column.
    /// </summary>
    _type: Ta_sqlany_data_type;
    /// <summary>
    ///  Indicates whether the last fetched data is NULL.
    ///  This field is only valid after a successful fetch operation.
    /// </summary>
    is_null: Tsacapi_bool;
    /// <summary>
    ///  The total number of bytes available to be fetched.
    ///  This field is only valid after a successful fetch operation.
    /// </summary>
    data_size: Tsize_t;
  end;
  Pa_sqlany_data_info = ^Ta_sqlany_data_info;

  /// <summary>
  ///  An enumeration of the callback types.
  /// </summary>
  Ta_sqlany_callback_type = (
    /// <summary>
    ///  This function is called just before a database request is sent to the server.
    ///  CALLBACK_START is used only on Windows operating systems.
    /// </summary>
    CALLBACK_START = 0,
    /// <summary>
    ///  This function is called repeatedly by the interface library while the database server or client library is busy processing your database request.
    /// </summary>
    CALLBACK_WAIT,
    /// <summary>
    ///  This function is called after the response to a database request has been received by the DBLIB interface DLL.
    ///  CALLBACK_FINISH is used only on Windows operating systems.
    /// </summary>
    CALLBACK_FINISH,
    /// <summary>
    ///  This function is called when messages are received from the server during the processing of a request.
    ///  Messages can be sent to the client application from the database server using the SQL MESSAGE statement.
    ///  Messages can also be generated by long running database server statements
    /// </summary>
    DB_CALLBACK_MESSAGE = 7,
    /// <summary>
    ///  This function is called when the database server is about to drop a connection because of a liveness timeout,
    ///  through a DROP CONNECTION statement, or because the database server is being shut down.
    ///  The connection name conn_name is passed in to allow you to distinguish between connections.
    ///  If the connection was not named, it has a value of NULL.
    /// </summary>
    CALLBACK_CONN_DROPPED,
    /// <summary>
    ///  This function is called once for each debug message and is passed a null-terminated string containing the text of the debug message.
    ///  A debug message is a message that is logged to the LogFile file. In order for a debug message to be passed to this callback, the LogFile
    ///  connection parameter must be used. The string normally has a newline character (\n) immediately before the terminating null character.
    /// </summary>
    CALLBACK_DEBUG_MESSAGE,
    /// <summary>
    ///  This function is called when a file transfer requires validation.
    ///  If the client data transfer is being requested during the execution of indirect statements such as from within a stored procedure,
    ///  the client library will not allow a transfer unless the client application has registered a validation callback and the response from
    ///  the callback indicates that the transfer may take place.
    /// </summary>
    CALLBACK_VALIDATE_FILE_TRANSFER
    );

  /// <summary>
  ///  An enumeration of the message types for the MESSAGE callback.
  /// </summary>
  Ta_sqlany_message_type = (
    /// <summary>
    ///  The message type was INFO.
    /// </summary>
    MESSAGE_TYPE_INFO = 0,
    /// <summary>
    ///  The message type was WARNING.
    /// </summary>
    MESSAGE_TYPE_WARNING,
    /// <summary>
    ///  The message type was ACTION.
    /// </summary>
    MESSAGE_TYPE_ACTION,
    /// <summary>
    ///  The message type was STATUS.
    /// </summary>
    MESSAGE_TYPE_STATUS,
    /// <summary>
    ///  Initializes the interface.
    ///  This type of message is generated by long running database server statements such as BACKUP DATABASE and LOAD TABLE.
    /// </summary>
    MESSAGE_TYPE_PROGRESS
  );

  PZSQLAnyDateTime = ^TZSQLAnyDateTime;
  TZSQLAnyDateTime = packed record
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

  TZSQLAnywherePlainDriver = class(TZAbstractPlainDriver)
  public
    /// <summary>
    ///  Initializes the interface.
    /// </summary>
    /// <param name="app_name">
    ///  A string that names the application that is using the API.  For example, "PHP", "PERL", or "RUBY".
    /// </param>
    /// <param name="api_version">
    ///  The version of the compiled application.
    /// </param>
    /// <param name="version_available">
    ///  An optional argument to return the maximum supported API version.
    /// </param>
    /// <returns>
    ///  1 on success, 0 otherwise.
    /// </returns>
    sqlany_init: function(app_name: PAnsiChar; api_version: Tsacapi_u32;
      version_available: Psacapi_u32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Initializes the interface.
    /// </summary>
    /// <param name="app_name">
    ///  A string that names the application that is using the API.  For example, "PHP", "PERL", or "RUBY".
    /// </param>
    /// <param name="api_version">
    ///  The version of the compiled application.
    ///  This should normally be one of the SQLANY_API_VERSION_* macros
    /// </param>
    /// <param name="version_available">
    ///  An optional argument to return the maximum supported API version.
    /// </param>
    /// <returns>
    ///  a context object on success and NULL on failure.
    /// </returns>
    sqlany_init_ex: function(app_name: PAnsiChar; api_version: Tsacapi_u32;
      version_available: Psacapi_u32): Pa_sqlany_interface_context; cdecl;
    /// <summary>
    ///  Finalizes the interface.
    ///  Frees any resources allocated by the API.
    /// </summary>
    sqlany_fini: procedure(); cdecl;
    /// <summary>
    ///  Finalize the interface that was created using the specified context.
    ///  Frees any resources allocated by the API.
    /// </summary>
    /// <param name="context">
    ///  A context object that was returned from sqlany_init_ex().
    /// </param>
    sqlany_fini_ex: procedure(context: Pa_sqlany_interface_context); cdecl;
    /// <summary>
    ///  Creates a connection object.
    ///  You must create an API connection object before establishing a database connection. Errors can be retrieved
    ///  from the connection object. Only one request can be processed on a connection at a time. In addition,
    ///  not more than one thread is allowed to access a connection object at a time. Undefined behavior or a failure
    ///  occurs when multiple threads attempt to access a connection object simultaneously.
    /// </summary>
    /// <returns>
    ///  A connection object.
    /// </returns>
    sqlany_new_connection: function: Pa_sqlany_connection; cdecl;
    /// <summary>
    ///  Creates a connection object using a context.
    ///  An API connection object needs to be created before a database connection is established. Errors can be retrieved
    ///  from the connection object. Only one request can be processed on a connection at a time. In addition,
    ///  not more than one thread is allowed to access a connection object at a time. If multiple threads attempt
    ///  to access a connection object simultaneously, then undefined behavior/crashes will occur.
    /// </summary>
    /// <param name="context">
    ///  A context object that was returned from sqlany_init_ex().
    /// </param>
    /// <returns>
    ///  A connection object.
    /// </returns>
    sqlany_new_connection_ex: function(context: Pa_sqlany_interface_context): Pa_sqlany_connection; cdecl;
    /// <summary>
    ///  Frees the resources associated with a connection object.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object created with sqlany_new_connection().
    /// </param>
    sqlany_free_connection: procedure(sqlany_conn: Pa_sqlany_connection); cdecl;
    /// <summary>
    ///  Creates a connection object based on a supplied DBLIB SQLCA pointer.
    /// </summary>
    /// <param name="arg">
    ///  A void * pointer to a DBLIB SQLCA object.
    /// </param>
    /// <returns>
    ///  A connection object.
    /// </returns>
    sqlany_make_connection: function(arg: Pointer): Pa_sqlany_connection; cdecl;
    /// <summary>
    ///  Creates a connection object based on a supplied DBLIB SQLCA pointer.
    /// </summary>
    /// <param name="context">
    ///  A valid context object that was created by sqlany_init_ex().
    /// </param>
    /// <param name="arg">
    ///  A void * pointer to a DBLIB SQLCA object.
    /// </param>
    /// <returns>
    ///  A connection object.
    /// </returns>
    sqlany_make_connection_ex: function(constext: Pa_sqlany_interface_context;
      arg: Pointer): Pa_sqlany_connection; cdecl;
    /// <summary>
    ///  Creates a connection to a SQL Anywhere database server using the supplied connection object and connection string.
    ///  The supplied connection object must first be allocated using sqlany_new_connection().
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object created by sqlany_new_connection().
    /// </param>
    /// <param name="str">
    ///  A SQL Anywhere connection string.
    /// </param>
    /// <returns>
    ///  1 if the connection is established successfully or 0 when the connection fails.
    ///  Use sqlany_error() to retrieve the error code and message.
    /// </returns>
    sqlany_connect: function(sqlany_conn: Pa_sqlany_connection; str: PAnsichar): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Disconnects an already established SQL Anywhere connection.
    ///  All uncommitted transactions are rolled back.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object with a connection established using sqlany_connect().
    /// </param>
    /// <returns>
    ///  1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_disconnect: function(sqlany_conn: Pa_sqlany_connection): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Cancel an outstanding request on a connection.
    ///  This function can be used to cancel an outstanding request on a specific connection.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object with a connection established using sqlany_connect().
    /// </param>
    sqlany_cancel: procedure(sqlany_conn: Pa_sqlany_connection); cdecl;
    /// <summary>
    ///  Executes the supplied SQL statement immediately without returning a result set.
    ///  This function is useful for SQL statements that do not return a result set.
    /// </summary>
    /// </param>
    /// <param name="sqlany_conn">
    ///  A connection object with a connection established using sqlany_connect().
    /// <param name="sql">
    ///  A string representing the SQL statement to be executed.
    /// </param>
    /// <returns>
    /// 1 on success or 0 on failure.
    /// </returns>
    sqlany_execute_immediate: function(sqlany_conn: Pa_sqlany_connection;
      sql: PAnsiChar): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Prepares a supplied SQL string
    ///  Execution does not happen until sqlany_execute() is
    ///  called. The returned statement object should be freed using sqlany_free_stmt()
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object with a connection established using sqlany_connect().
    /// <param name="sql">
    ///  The SQL statement to be prepared.
    /// </param>
    /// <returns>
    ///  A handle to a SQL Anywhere statement object or NULL if an error occours.
    ///  The statement object can be used by sqlany_execute() to execute the statement.
    /// </returns>
    sqlany_prepare: function(sqlany_conn: Pa_sqlany_connection;
      sql: PAnsiChar): Pa_sqlany_stmt; cdecl;
    /// <summary>
    ///  Frees resources associated with a prepared statement object
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object returned by the successful execution of sqlany_prepare() or sqlany_execute_direct().
    /// </param>
    sqlany_free_stmt: procedure(sqlany_stmt: Pa_sqlany_stmt); cdecl;
    /// <summary>
    ///  Returns the number of parameters expected for a prepared statement
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object returned by the successful execution of sqlany_prepare().
    /// </param>
    /// <returns>
    ///  The expected number of parameters, or -1 if the statement object is not valid.
    /// </returns>
    sqlany_num_params: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_i32; cdecl;
    /// <summary>
    ///   Describes the bind parameters of a prepared statement
    ///  This function allows the caller to determine information about prepared statement parameters.  The type of prepared
    ///  statement, stored procedured or a DML, determines the amount of information provided.  The direction of the parameters
    ///  (input, output, or input-output) are always provided.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="index">
    ///  The index of the parameter. This number must be between 0 and sqlany_num_params() - 1.
    /// <param name="param">
    ///  An a_sqlany_bind_param structure that is populated with information.
    /// </param>
    /// <returns>
    ///  1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_describe_bind_param: function(sqlany_stmt: Pa_sqlany_stmt;
      index: Tsacapi_u32; param: Pa_sqlany_bind_param): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Bind a user-supplied buffer as a parameter to the prepared statement.
    /// </summary>
    /// </param>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// <param name="index">
    ///  The index of the parameter. This number must be between 0 and sqlany_num_params() - 1.
    /// <param name="param">
    ///  An a_sqlany_bind_param structure description of the parameter to be bound.
    /// </param>
    /// <returns>
    ///  1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_bind_param: function(sqlany_stmt: Pa_sqlany_stmt;
      index: Tsacapi_u32; param: Pa_sqlany_bind_param): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Sends data as part of a bound parameter.
    ///  This method can be used to send a large amount of data for a bound parameter in chunks^.
    ///  This method can be used only when the batch size is 1.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="index">
    ///  The index of the parameter. This number must be between 0 and sqlany_num_params() - 1.
    /// </param>
    /// <param name="buffer">
    ///  The data to be sent.
    /// </param>
    /// <param name="size">
    ///  The number of bytes to send.
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure.
    /// </returns>
    sqlany_send_param_data: function(sqlany_stmt: Pa_sqlany_stmt;
      index: Tsacapi_u32; buffer: PAnsiChar; size: Tsize_t): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Clears param data that was previously been set using \sa sqlany_send_param_data().
    ///  This method can be used to clear data that was previously been sent using sqlany_send_param_data()
    ///  If no param data was previously sent, nothing is changed.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="index">
    ///  param index The index of the parameter. This should be a number between 0 and sqlany_num_params() - 1.
    /// </param>
    /// <param name="buffer">
    ///  The data to be sent.
    /// </param>
    /// <param name="size">
    ///  The number of bytes to send.
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure.
    /// </returns>
    sqlany_reset_param_data: function(sqlany_stmt: Pa_sqlany_stmt;
      index: Tsacapi_u32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves the length of the last error message stored in the connection object
    ///  including the NULL terminator. If there is no error, 0 is returned.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object returned from sqlany_new_connection().
    /// </param>
    /// <returns>
    ///  The length of the last error message including the NULL terminator.
    /// </returns>
    sqlany_error_length: function(sqlany_conn: Pa_sqlany_connection): Tsize_t; cdecl;
    /// <summary>
    ///  Sets the size of the row array for a batch execute
    ///  The batch size is used only for an INSERT statement. The default batch size is 1.
    ///  A value greater than 1 indicates a wide insert.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="num_rows">
    ///  The number of rows for batch execution. The value must be 1 or greater.
    /// </param>
    /// <returns>
    ///  return 1 on success or 0 on failure.
    /// </returns>
    sqlany_set_batch_size: function(sqlany_stmt: Pa_sqlany_stmt;
      num_rows: Tsacapi_u32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Sets the bind type of parameters.
    ///  The default value is 0, which indicates column-wise binding. A non-zero value indicates
    ///  row-wise binding and specifies the byte size of the data structure that stores the row.
    ///  The parameter is bound to the first element in a contiguous array of values. The address
    ///  offset to the next element is computed based on the bind type:
    ///
    ///  Column-wise binding - the byte size of the parameter type
    ///  Row-wise binding - the row_size
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="row_size">
    ///  The byte size of the row. A value of 0 indicates column-wise binding and a positive value indicates row-wise binding.
    /// </param>
    /// <returns>
    ///  return 1 on success or 0 on failure.
    /// </returns>
    sqlany_set_param_bind_type: function(sqlany_stmt: Pa_sqlany_stmt; row_size: Tsize_t): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves the size of the row array for a batch execute.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <returns>
    ///  The size of the row array.
    /// </returns>
    sqlany_get_batch_size: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_u32; cdecl;
    /// <summary>
    ///  Sets the size of the row set to be fetched by the sqlany_fetch_absolute() and sqlany_fetch_next() functions.
    ///  The default size of the row set is 1. Specifying num_rows to be a value greater than 1 indicates a wide fetch.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="num_rows">
    ///  The size of the row set. The value must be 1 or greater.
    /// </param>
    /// <returns>
    ///  return 1 on success or 0 on failure.
    /// </returns>
    sqlany_set_rowset_size: function(sqlany_stmt: Pa_sqlany_stmt;
      num_rows: Tsacapi_u32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves the size of the row set to be fetched by the sqlany_fetch_absolute() and sqlany_fetch_next() functions.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <returns>
    ///  The size of the row set, or 0 if the statement does not return a result set.
    /// </returns>
    sqlany_get_rowset_size: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_u32; cdecl;
    /// <summary>
    ///  Sets the bind type of columns.
    ///  The default value is 0, which indicates column-wise binding. A non-zero value indicates
    ///  row-wise binding and specifies the byte size of the data structure that stores the row.
    ///  The column is bound to the first element in a contiguous array of values. The address
    ///  offset to the next element is computed based on the bind type:
    ///
    ///  Column-wise binding - the byte size of the column type
    ///  Row-wise binding - the row_size
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="row_size">
    ///  The byte size of the row. A value of 0 indicates column-wise binding and a positive value indicates row-wise binding.
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure.
    /// </returns>
    sqlany_set_column_bind_type: function(sqlany_stmt: Pa_sqlany_stmt; row_size: Tsacapi_u32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Binds a user-supplied buffer as a result set column to the prepared statement.
    ///  If the size of the fetched row set is greater than 1, the buffer must be large enough to
    ///  hold the data of all of the rows in the row set. This function can also be used to clear the
    ///  binding of a column by specifying value to be NULL.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="index">
    ///  The index of the column. This number must be between 0 and sqlany_num_cols() - 1.
    /// </param>
    /// <param name="value">
    ///  An a_sqlany_data_value structure describing the bound buffers, or NULL to clear previous binding information.
    /// </param>
    /// <returns>
    ///  1 on success or 0 on unsuccessful.
    /// </returns>
    sqlany_bind_column: function(sqlany_stmt: Pa_sqlany_stmt; index: Tsacapi_u32;
      value: Pa_sqlany_data_valueV4up): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Removes all column bindings defined using sqlany_bind_column().
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure.
    /// </returns>
    sqlany_clear_column_bindings: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Returns the number of rows fetched.
    ///  In general, the number of rows fetched is equal to the size specified by the sqlany_set_rowset_size() function. The
    ///  exception is when there are fewer rows from the fetch position to the end of the result set than specified, in which
    ///  case the number of rows fetched is smaller than the specified row set size. The function returns -1 if the last fetch
    ///  was unsuccessful or if the statement has not been executed. The function returns 0 if the statement has been executed
    ///  but no fetching has been done.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <returns>
    ///  The number of rows fetched or -1 on failure
    /// </returns>
    sqlany_fetched_rows: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_i32; cdecl;
    /// <summary>
    ///  Sets the current row in the fetched row set.
    ///  When a sqlany_fetch_absolute() or sqlany_fetch_next() function is executed, a row set
    ///  is created and the current row is set to be the first row in the row set. The functions
    ///  sqlany_get_column(), sqlany_get_data(), sqlany_get_data_info() are used to retrieve data
    ///  at the current row.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="row_num">
    ///  The row number within the row set. The valid values are from 0 to sqlany_fetched_rows() - 1.
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure
    /// </returns>
    sqlany_set_rowset_pos: function(sqlany_stmt: Pa_sqlany_stmt; row_num: Tsacapi_u32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Resets a statement to its prepared state condition.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure
    /// </returns>
    sqlany_reset: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves information about the parameters that were bound using sqlany_bind_param().
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <param name="index">
    ///  The index of the parameter. This number should be between 0 and sqlany_num_params() - 1.
    /// </param>
    /// <returns>
    ///  1 on success or 0 on failure
    /// </returns>
    sqlany_get_bind_param_info: function(sqlany_stmt: Pa_sqlany_stmt;
      index: Tsacapi_u32; info: Pa_sqlany_bind_param_info): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Executes a prepared statement.
    ///  You can use sqlany_num_cols() to verify if the executed statement returned a result set.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement prepared successfully using sqlany_prepare().
    /// </param>
    /// <returns>
    ///   1 if the statement is executed successfully or 0 on failure.
    /// </returns>
    sqlany_execute: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Executes the SQL statement specified by the string argument and possibly returns a result set.
    ///  Use this method to prepare and execute a statement.
    ///  or instead of calling sqlany_prepare() followed by sqlany_execute()
    ///  This function cannot be used for executing a SQL statement with parameters.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object with a connection established using sqlany_connect().
    /// </param>
    /// <param name="sql_str">
    ///  A SQL string. The SQL string should not have parameters such as ?.
    /// </param>
    /// <returns>
    ///   A statement handle if the function executes successfully, NULL when the function executes unsuccessfully.
    /// </returns>
    sqlany_execute_direct: function(sqlany_conn: Pa_sqlany_connection;
      sql_str: PAnsiChar): Pa_sqlany_stmt; cdecl;
    /// <summary>
    ///  Moves the current row in the result set to the specified row number and then fetches
    ///  rows of data starting from the current row.
    ///  The number of rows fetched is set using the sqlany_set_rowset_size() function. By default, one row is returned.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <param name="row_num">
    ///  The row number to be fetched. The first row is 1, the last row is -1.
    /// </param>
    /// <returns>
    ///   1 if the fetch was successfully, 0 when the fetch is unsuccessful.
    /// </returns>
    sqlany_fetch_absolute: function(sqlany_stmt: Pa_sqlany_stmt;
      row_num: Tsacapi_i32): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Returns the next set of rows from the result set.
    ///  When the result object is first created, the current row
    ///  pointer is set to before the first row, that is, row 0.
    ///  This function first advances the row pointer to the next
    ///  unfetched row and then fetches rows of data starting from
    ///  that row. The number of rows fetched is set by the
    ///  sqlany_set_rowset_size() function. By default, one row is returned.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <returns>
    ///   1 if the fetch was successfully, 0 when the fetch is unsuccessful.
    /// </returns>
    sqlany_fetch_next: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Advances to the next result set in a multiple result set query.
    ///  If a query (such as a call to a stored procedure) returns multiple result sets, then this function
    ///  advances from the current result set to the next.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <returns>
    ///   1 if the statement successfully advances to the next result set, 0 otherwise.
    /// </returns>
    sqlany_get_next_result: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Returns the number of rows affected by execution of the prepared statement.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <returns>
    ///   The number of rows affected or -1 on failure.
    /// </returns>
    sqlany_affected_rows: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_i32; cdecl;
    /// <summary>
    ///  Returns number of columns in the result set.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <returns>
    ///  The number of columns in the result set or -1 on a failure.
    /// </returns>
    sqlany_num_cols: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_i32; cdecl;
    /// <summary>
    ///  By default this function only returns an estimate. To return an exact count, set the row_counts option
    ///  on the connection.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <returns>
    ///  The number rows in the result set. If the number of rows is an estimate, the number returned is
    ///  negative and the estimate is the absolute value of the returned integer. The value returned is positive
    ///  if the number of rows is exact.
    /// </returns>
    sqlany_num_rows: function(sqlany_stmt: Pa_sqlany_stmt): Tsacapi_i32; cdecl;
    /// <summary>
    ///  When a sqlany_fetch_absolute() or sqlany_fetch_next() function is executed, a row set
    ///  is created and the current row is set to be the first row in the row set. The current
    ///  row is set using the sqlany_set_rowset_pos() function.
    ///  For A_BINARY and A_STRING * data types,
    ///  value->buffer points to an internal buffer associated with the result set.
    ///  Do not rely upon or alter the content of the pointer buffer as it changes when a
    ///  new row is fetched or when the result set object is freed.  Users should copy the
    ///  data out of those pointers into their own buffers.
    ///  The value->length field indicates the number of valid characters that
    ///  value->buffer points to. The data returned in value->buffer is not
    ///  null-terminated. This function fetches all the returned values from the SQL
    ///  Anywhere database server.  For example, if the column contains
    ///  a blob, this function attempts to allocate enough memory to hold that value.
    ///  If you do not want to allocate memory, use sqlany_get_data() instead.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <param name="col_index">
    ///  The number of the column to be retrieved. The column number is between 0 and sqlany_num_cols() - 1.
    /// </param>
    /// <param name="buffer">
    ///  An a_sqlany_data_value object to be filled with the data fetched for column col_index at the current row in the row set.
    /// </param>
    /// <returns>
    ///  1 on success or 0 for failure. A failure can happen if any of the parameters are invalid or if there is
    ///  not enough memory to retrieve the full value from the SQL Anywhere database server.
    ///  if the number of rows is exact.
    /// </returns>
    sqlany_get_column: function(sqlany_stmt: Pa_sqlany_stmt; col_index: Tsacapi_u32;
      buffer: Pa_sqlany_data_value): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves the data fetched for the specified column at the current row into the supplied buffer memory
    ///  When a sqlany_fetch_absolute() or sqlany_fetch_next() function is executed, a row set
    ///  is created and the current row is set to be the first row in the row set. The current
    ///  row is set using the sqlany_set_rowset_pos() function.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <param name="col_index">
    ///  The number of the column to be retrieved. The column number is between 0 and sqlany_num_cols() - 1.
    /// </param>
    /// <param name="offset">
    ///  The starting offset of the data to get.
    /// </param>
    /// <param name="buffer">
    ///  A buffer to be filled with the contents of the column at the current row in the row set.
    ///  The buffer pointer must be aligned correctly for the data type copied into it.
    /// </param>
    /// <param name="size">
    ///  The size of the buffer in bytes. The function fails. The function fails
    ///   if you specify a size greater than 2^31 - 1
    /// </param>
    /// <returns>
    ///  The number of bytes successfully copied into the supplied buffer.
    ///  This number must not exceed 2^31 - 1. 0 indicates that no data remains to be copied. -1 indicates a failure.
    ///  if the number of rows is exact.
    /// </returns>
    sqlany_get_data: function(sqlany_stmt: Pa_sqlany_stmt; col_index: Tsacapi_u32;
      offset: Tsize_t; Buffer: Pointer; size: Tsize_t): Tsacapi_i32; cdecl;
    /// <summary>
    ///  Retrieves information about the fetched data at the current row
    ///  When a sqlany_fetch_absolute() or sqlany_fetch_next() function is executed, a row set
    ///  is created and the current row is set to be the first row in the row set. The current
    ///  row is set using the sqlany_set_rowset_pos() function.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <param name="col_index">
    ///  The number of the column to be retrieved. The column number is between 0 and sqlany_num_cols() - 1.
    /// </param>
    /// <param name="buffer">
    ///  A data info buffer to be filled with the metadata about the data at the current row in the row set.
    /// </param>
    /// <returns>
    ///   1 on success, and 0 on failure. Failure is returned when any of the supplied parameters are invalid.
    /// </returns>
    sqlany_get_data_info: function(sqlany_stmt: Pa_sqlany_stmt; col_index: Tsacapi_u32;
      buffer: Pa_sqlany_data_info): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves column metadata information and fills the a_sqlany_column_info structure with information about the column.
    /// </summary>
    /// <param name="sqlany_stmt">
    ///  A statement object that was executed by sqlany_execute() or sqlany_execute_direct().
    /// </param>
    /// <param name="col_index">
    ///  The number of the column to be retrieved. The column number is between 0 and sqlany_num_cols() - 1.
    /// </param>
    /// <param name="buffer">
    ///  A column info structure to be filled with column information.
    /// </param>
    /// <returns>
    ///   1 on success or 0 if the column index is out of range, or if the statement does not return a result set.
    /// </returns>
    sqlany_get_column_info: function(sqlany_stmt: Pa_sqlany_stmt; col_index: Tsacapi_u32;
      buffer: Pa_sqlany_column_info): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Commits the current transaction.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  The connection object on which the commit operation is performed.
    /// <returns>
    ///   return 1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_commit: function(sqlany_conn: Pa_sqlany_connection): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Rolls back the current transaction.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  The connection object on which the rollback operation is to be performed.
    /// </param>
    /// <returns>
    ///   return 1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_rollback: function(sqlany_conn: Pa_sqlany_connection): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Returns the current client version.
    ///  This method fills the buffer passed with the major, minor, patch, and build number of the client library.
    ///  The buffer will be null-terminated.
    /// </summary>
    /// <param name="buffer">
    ///  The buffer to be filled with the client version string.
    /// </param>
    /// <returns>
    /// <param name="len">
    ///  The length of the buffer supplied.
    /// <returns>
    ///   1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_client_version: function(buffer: PAnsiChar; len: Tsize_t): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Returns the current client version.
    ///  This method fills the buffer passed with the major, minor, patch, and build number of the client library.
    ///  The buffer will be null-terminated.
    /// </summary>
    /// <param name="context">
    ///  The object that was created with sqlany_init_ex().
    /// </param>
    /// <param name="buffer">
    ///  The buffer to be filled with the client version string.
    /// </param>
    /// <param name="len">
    ///  The length of the buffer supplied.
    /// </param>
    /// <returns>
    ///   1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_client_version_ex: function(context: Pa_sqlany_interface_context;
      buffer: PAnsiChar; len: Tsize_t): Tsacapi_bool; cdecl;
    /// <summary>
    ///  Retrieves the last error code and message stored in the connection object.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object returned from sqlany_new_connection().
    /// </param>
    /// <param name="buffer">
    ///  A buffer to be filled with the error message.
    /// </param>
    /// <param name="size">
    ///  The size of the supplied buffer.
    /// </param>
    /// <returns>
    ///   The last error code. Positive values are warnings, negative values are errors, and 0 indicates success.
    /// </returns>
    sqlany_error: function(sqlany_conn: Pa_sqlany_connection;
      buffer: PAnsiChar; size: Tsize_t): Tsacapi_i32; cdecl;
    /// <summary>
    ///  Retrieves the current SQLSTATE.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object returned from sqlany_new_connection().
    /// </param>
    /// <param name="buffer">
    ///  A buffer to be filled with the current 5-character SQLSTATE.
    /// </param>
    /// <param name="size">
    ///  The buffer size.
    /// </param>
    /// <returns>
    ///   The number of bytes copied into the buffer.
    /// </returns>
    sqlany_sqlstate: function(sqlany_conn: Pa_sqlany_connection;
      buffer: PAnsiChar; size: Tsize_t): Tsize_t; cdecl;
    /// <summary>
    ///   Clears the last stored error code
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object returned from sqlany_new_connection().
    /// </param>
    sqlany_clear_error: procedure(sqlany_conn: Pa_sqlany_connection); cdecl;
    /// <summary>
    ///  Register a callback routine.
    ///  This function can be used to register callback functions.
    /// </summary>
    /// <param name="sqlany_conn">
    ///  A connection object with a connection established using sqlany_connect().
    /// </param>
    /// <param name="index">
    ///  Any of the callback types listed below.
    /// </param>
    /// <param name="callback">
    ///  Address of the callback routine.
    /// </param>
    /// <returns>
    ///   1 when successful or 0 when unsuccessful.
    /// </returns>
    sqlany_register_callback: function(sqlany_conn: Pa_sqlany_connection;
      index: Ta_sqlany_callback_type; callback: TSQLANY_CALLBACK): Tsacapi_bool; cdecl;
  protected
    procedure LoadApi; override;
  public
    procedure LoadCodePages; override;
    constructor Create;

    function Clone: IZPlainDriver; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

{$ENDIF ZEOS_DISABLE_SQLANY}
implementation
{$IFNDEF ZEOS_DISABLE_SQLANY}

uses ZPlainLoader, ZEncoding;

{ TZSQLAnywherePlainDriver }

function TZSQLAnywherePlainDriver.Clone: IZPlainDriver;
begin
  Result := TZSQLAnywherePlainDriver.Create;
end;

constructor TZSQLAnywherePlainDriver.Create;
begin
  FLoader := TZNativeLibraryLoader.Create([]);
  FLoader.AddLocation(SQLAnyLibrary);
  LoadCodePages;
end;

function TZSQLAnywherePlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Sybase SQL Anywhere';
end;

function TZSQLAnywherePlainDriver.GetProtocol: string;
begin
  Result := 'asa_capi';
end;

procedure TZSQLAnywherePlainDriver.LoadApi;
begin
  with FLoader do
  begin
    @sqlany_init                  := GetAddress('sqlany_init');
    @sqlany_init_ex               := GetAddress('sqlany_init_ex');
    @sqlany_fini                  := GetAddress('sqlany_fini', true);
    @sqlany_fini_ex               := GetAddress('sqlany_fini_ex');
    @sqlany_new_connection        := GetAddress('sqlany_new_connection');
    @sqlany_new_connection_ex     := GetAddress('sqlany_new_connection_ex');
    @sqlany_free_connection       := GetAddress('sqlany_free_connection');
    @sqlany_make_connection       := GetAddress('sqlany_make_connection');
    @sqlany_make_connection_ex    := GetAddress('sqlany_make_connection_ex');
    @sqlany_connect               := GetAddress('sqlany_connect');
    @sqlany_disconnect            := GetAddress('sqlany_disconnect');
    @sqlany_cancel                := GetAddress('sqlany_cancel');
    @sqlany_execute_immediate     := GetAddress('sqlany_execute_immediate');
    @sqlany_prepare               := GetAddress('sqlany_prepare');
    @sqlany_free_stmt             := GetAddress('sqlany_free_stmt');
    @sqlany_num_params            := GetAddress('sqlany_num_params');
    @sqlany_describe_bind_param   := GetAddress('sqlany_describe_bind_param');
    @sqlany_bind_param            := GetAddress('sqlany_bind_param');
    @sqlany_send_param_data       := GetAddress('sqlany_send_param_data');
    @sqlany_reset_param_data      := GetAddress('sqlany_reset_param_data');
    @sqlany_error_length          := GetAddress('sqlany_error_length');
    @sqlany_set_batch_size        := GetAddress('sqlany_set_batch_size');
    @sqlany_set_param_bind_type   := GetAddress('sqlany_set_param_bind_type');
    @sqlany_get_batch_size        := GetAddress('sqlany_get_batch_size');
    @sqlany_set_rowset_size       := GetAddress('sqlany_set_rowset_size');
    @sqlany_get_rowset_size       := GetAddress('sqlany_get_rowset_size');
    @sqlany_set_column_bind_type  := GetAddress('sqlany_set_column_bind_type');
    @sqlany_bind_column           := GetAddress('sqlany_bind_column');
    @sqlany_clear_column_bindings := GetAddress('sqlany_clear_column_bindings');
    @sqlany_fetched_rows          := GetAddress('sqlany_fetched_rows');
    @sqlany_set_rowset_pos        := GetAddress('sqlany_set_rowset_pos');
    @sqlany_reset                 := GetAddress('sqlany_reset');
    @sqlany_get_bind_param_info   := GetAddress('sqlany_get_bind_param_info');
    @sqlany_execute               := GetAddress('sqlany_execute');
    @sqlany_fetch_absolute        := GetAddress('sqlany_fetch_absolute');
    @sqlany_fetch_next            := GetAddress('sqlany_fetch_next');
    @sqlany_get_next_result       := GetAddress('sqlany_get_next_result');
    @sqlany_affected_rows         := GetAddress('sqlany_affected_rows');
    @sqlany_num_cols              := GetAddress('sqlany_num_cols', True);
    @sqlany_num_rows              := GetAddress('sqlany_num_rows');
    @sqlany_get_column            := GetAddress('sqlany_get_column');
    @sqlany_get_data              := GetAddress('sqlany_get_data');
    @sqlany_get_data_info         := GetAddress('sqlany_get_data_info');
    @sqlany_get_column_info       := GetAddress('sqlany_get_column_info');
    @sqlany_commit                := GetAddress('sqlany_commit');
    @sqlany_rollback              := GetAddress('sqlany_rollback');
    @sqlany_client_version        := GetAddress('sqlany_client_version');
    @sqlany_client_version_ex     := GetAddress('sqlany_client_version_ex');
    @sqlany_error                 := GetAddress('sqlany_error');
    @sqlany_sqlstate              := GetAddress('sqlany_sqlstate');
    @sqlany_clear_error           := GetAddress('sqlany_clear_error');
    @sqlany_register_callback     := GetAddress('sqlany_register_callback');
  end;
end;

procedure TZSQLAnywherePlainDriver.LoadCodePages;
begin
  { MultiByte }
  AddCodePage('TIS-620', 1, ceAnsi, 874);
  AddCodePage('Windows-31J', 2, ceAnsi, 932);
  AddCodePage('GBK', 3, ceAnsi, 936);
  AddCodePage('IBM949', 4, ceAnsi, 949);
  AddCodePage('BIG5', 5, ceAnsi, 950);
  AddCodePage('EUC_CHINA', 6, ceAnsi, zCP_GB2312);
  AddCodePage('UTF-8', 7, ceUTF8, zCP_UTF8, '', 3);

  { SingleByte }
  AddCodePage('Windows-1250', 8, ceAnsi, 1250);
  AddCodePage('Windows-1251', 9, ceAnsi, 1251);
  AddCodePage('Windows-1252', 10, ceAnsi, 1252);
  AddCodePage('Windows-1253', 11, ceAnsi, 1253);
  AddCodePage('Windows-1254', 12, ceAnsi, 1254);
  AddCodePage('Windows-1255', 13, ceAnsi, 1255);
  AddCodePage('Windows-1256', 14, ceAnsi, 1256);
  AddCodePage('Windows-1257', 15, ceAnsi, 1257);
  AddCodePage('Windows-1258', 16, ceAnsi, 1258);
  {*nix}
  AddCodePage('ISO_8859-6:1987', 17, ceAnsi, 1256);
  AddCodePage('ISO_8859-2:1987', 18, ceAnsi, 1251);
  AddCodePage('GB2312', 19, ceAnsi, zCP_GB2312);
  AddCodePage('Big5-HKSCS', 20, ceAnsi, 950);
  AddCodePage('ISO_8859-9:1989', 21, ceAnsi, 920);
end;

initialization
{$ENDIF ZEOS_DISABLE_SQLANY}
end.
