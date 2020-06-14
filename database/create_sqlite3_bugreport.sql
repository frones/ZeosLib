/*==============================================================*/
/* Tables for Ticket #386                                       */
/*==============================================================*/

CREATE TABLE table_ticket_386 (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  Field_1 Varchar(10),
  Field_2 Int
);

CREATE TABLE [TestSF431] (
  [datetime] REAL NOT NULL,
  [username] VARCHAR(50) NOT NULL,
  [device_id] INT NOT NULL,
  [pin] TINYINT,
  [byte2] TINYINT,
  [byte3] TINYINT,
  [byte4] TINYINT,
  [byte5] TINYINT,
  [val] FLOAT DEFAULT 0,
  [is_received_from_serial] BOOLEAN DEFAULT 0,
  [is_received_from_ip] BOOLEAN DEFAULT 0,
  [IP] VARCHAR NOT NULL DEFAULT 'N/A',
  [sec_between] INTEGER NOT NULL DEFAULT (-1),
  [io] TINYINT DEFAULT 0,
  [object_name] VARCHAR,
  [alarm] BOOLEAN DEFAULT false
);