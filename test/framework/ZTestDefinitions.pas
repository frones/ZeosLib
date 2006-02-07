{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       Test Class Definitions for Testing Framework      }
{                                                         }
{    Copyright (c) 1999-2004 Zeos Development Group       }
{    Written by Sergey Merkuriev, Sergey Seroukhov        }
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

unit ZTestDefinitions;

interface

{$I ZTestFramework.inc}

uses
  ZTestCase, ZSqlTestCase;

type
  {** Implements a generic test case for core category. }
  TZCoreGenericTestCase = class (TZGenericTestCase);

  {** Implements a portable SQL test case for core category. }
  TZCorePortableSQLTestCase = class (TZPortableSQLTestCase);

  {** Implements a specific SQL test case for core category. }
  TZCoreSpecificSQLTestCase = class (TZSpecificSQLTestCase);

  {** Implements a generic test case for parsesql category. }
  TZParseSQLGenericTestCase = class (TZGenericTestCase);

  {** Implements a portable SQL test case for parsesql category. }
  TZParseSQLPortableSQLTestCase = class (TZPortableSQLTestCase);

  {** Implements a specific SQL test case for parsesql category. }
  TZParseSQLSpecificSQLTestCase = class (TZSpecificSQLTestCase);

  {** Implements a portable SQL test case for plain category. }
  TZPlainPortableSQLTestCase = class (TZPortableSQLTestCase);

  {** Implements a specific SQL test case for plain category. }
  TZPlainSpecificSQLTestCase = class (TZSpecificSQLTestCase);

  {** Implements a generic test case for dbc category. }
  TZDbcGenericTestCase = class (TZGenericTestCase);

  {** Implements a portable SQL test case for dbc category. }
  TZDbcPortableSQLTestCase = class (TZPortableSQLTestCase);

  {** Implements a specific SQL test case for dbc category. }
  TZDbcSpecificSQLTestCase = class (TZSpecificSQLTestCase);

  {** Implements a portable SQL test case for component category. }
  TZComponentPortableSQLTestCase = class (TZPortableSQLTestCase);

  {** Implements a specific SQL test case for component category. }
  TZComponentSpecificSQLTestCase = class (TZSpecificSQLTestCase);

implementation
end.
