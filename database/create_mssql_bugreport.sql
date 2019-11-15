/*==============================================================*/
/* Procedure for Bug#907497                                     */
/*==============================================================*/
CREATE PROCEDURE proc907497
	@zzz varchar(60) out
AS
        set @zzz='7890'
	if @zzz='12345'
	begin
	 set @zzz='99999'
	end
GO

/*==============================================================*/
/* Tables and procedures for Bug#959307                         */
/*==============================================================*/

CREATE TABLE table959307 (
	id int identity not null,
	fld1 Varchar(10)
)
go


CREATE PROCEDURE proc959307 (@p varchar(10)) as
	delete from table959307
	insert into table959307 (fld1) values (@p)
GO

/*==============================================================*/
/* Tables Manntis#54                                            */
/*==============================================================*/
CREATE TABLE mantis54 (
    Key1 int NOT NULL ,
    BI bigint NULL ,
    F float NULL
)
go

/*==============================================================*/
/* Table : national_char_values                                 */
/*==============================================================*/
create table national_char_values (
n_id                 int                  not null,
s_nchar              nchar(255)           null,
s_nvarchar           nvarchar(255)        null,
b_ntext              ntext                null,
s_char               char(255)            null,
s_varchar            varchar(255)         null,
b_text               text                 null,
primary key  (n_id)
)
go

CREATE TABLE Mantis164 (
  [CardID] varchar(30) NOT NULL,
  [CardTypeID] smallint NOT NULL,
  [PublishDate] datetime NOT NULL,
  [UserID] uniqueidentifier NULL DEFAULT NEWID(),
  [AccountsID] uniqueidentifier NOT NULL DEFAULT NEWID(),
  [Deposited] bit NOT NULL,
  [bBinary] binary(16) null,
  [bVarBinary] varbinary(16) null,
  [bImage] image null,
  CONSTRAINT [PK_CD_CardInfo] PRIMARY KEY CLUSTERED ([CardID], [CardTypeID])
)
ON [PRIMARY]
GO

CREATE TABLE TableTicked306 (
  [ID] int NOT NULL PRIMARY KEY,
  [val1] nvarchar(200) default null,
  [val2] nvarchar(255) default (null)
)
GO
