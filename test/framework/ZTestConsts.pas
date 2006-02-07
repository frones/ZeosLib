{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Constants for Testing Framework              }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{             Written by Sergey Seroukhov                 }
{                                                         }
{*********************************************************}

{*********************************************************}
{ License Agreement:                                      }
{                                                         }
{ This library is free software; you can redistribute     }
{ it and/or modify it under the terms of the GNU Lesser   }
{ General Public License as published by the Free         }
{ Software Foundation; either version 2.1 of the License, }
{ or (at your option) any later version.                  }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ You should have received a copy of the GNU Lesser       }
{ General Public License along with this library; if not, }
{ write to the Free Software Foundation, Inc.,            }
{ 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA }
{                                                         }
{ The project web site is located on:                     }
{   http://www.sourceforge.net/projects/zeoslib.          }
{   http://www.zeoslib.sourceforge.net                    }
{                                                         }
{                                 Zeos Development Group. }
{*********************************************************}

unit ZTestConsts;

interface

const
  {** A special number for test inserted row. }
  TEST_ROW_ID = 32767;

  {** Buffer size for blob field }
  BINARY_BUFFER_SIZE = 1024;

  {** The bytes string/array length }
  BYTES_LEN = 10;

  {** The bytes filed value string/array length }
  BYTES_FIELD_LEN = 255;

const
  ROWS_COUNT = 10;
  MAX_ELEMENT = 100; //10000;
  MAX_POS_ELEMENT = 1000;

const
  { Constants for performance tests. }
  PERFORMANCE_TABLE_NAME = 'high_load';
  PERFORMANCE_PRIMARY_KEY = 'hl_id';

implementation

end.
 
