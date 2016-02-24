/*==============================================================*/
/* Tables for Bug#815852                                       */
/*==============================================================*/

CREATE DOMAIN tinteger int4 not null;
CREATE DOMAIN tstring varchar(35);

CREATE TABLE test815852 (
  fld1 tinteger,
  fld2 tstring
);

/*==============================================================*/
/* Tables for Bug#824780                                       */
/*==============================================================*/

CREATE SCHEMA xyz;

CREATE TABLE xyz.test824780 (
  fld1 int4 not null,
  fld2 varchar(35)
);

CREATE TABLE test824780 (
  fld1 int4 not null,
  fld2 varchar(35)
);

/*==============================================================*/
/* Tables for Bug#1014416                                       */
/*==============================================================*/

CREATE TABLE test1014416 (
  fld1 cidr,
  fld2 inet,
  fld3 macaddr
);

INSERT INTO test1014416 VALUES ('192.168.100.128/25',
  '192.168.100.128/25', '08:00:2b:01:02:03');
INSERT INTO test1014416 VALUES ('2001:4f8:3:ba:2e0:81ff:fe22:d1f1/128',
  '2001:4f8:3:ba:2e0:81ff:fe22:d1f1/128', '08-00-2b-01-02-03');
  
/*==============================================================*/
/* Table for SF-Bug#81                                          */
/* See https://sourceforge.net/p/zeoslib/tickets/81/            */
/*==============================================================*/

CREATE TABLE "RANMeter"
(
  "ID" bigserial NOT NULL,
  "MeterID" bigint,
  "RANID" bigint,
  "DateReceived" timestamp without time zone,
  "ReportedFaultRAN" character varying,
  "FunctionalTestResultID" integer,
  "CommunicationsTestResultID" integer,
  "AccuracyTestResultID" integer,
  "PrimaryFaultID" integer,
  "PrimaryFaultCauseID" integer,
  "PrimaryFaultResolutionID" integer,
  "SecondaryFaultID" integer,
  "SecondaryFaultCauseID" integer,
  "SecondaryFaultResolutionID" integer,
  "OtherFaultID" integer,
  "OtherFaultCauseID" integer,
  "OtherFaultResolutionID" integer,
  "TechnicianID" integer,
  "ReportedFaultTAG" character varying,
  "DateTested" timestamp without time zone,
  "DateRepaired" timestamp without time zone,
  "Notes" character varying,
  "TimeDrift" interval,
  "EnergyRegisterA" real,
  "EnergyRegisterB" real,
  "EnergyRegisterC" real,
  "StatusCode" character varying,
  "ReplacementPropertyNo" character varying,
  "DateReturned" timestamp without time zone,
  "StatusID" integer,
  "Received" boolean,
  "Hidden" boolean,
  "PartsUsed" character varying,
  "TagTypeID" integer,
  "CLEMID" integer,
  "ReportedFaultTAGID" integer,
  "F1" character varying,
  "F2" character varying,
  "RepairTypeID" integer,
  "NATAReportNo" character varying,
  "NICFirmwareID" integer,
  "DirectImport" boolean,
  "PalletID" integer,
  CONSTRAINT "RANMeter_pkey" PRIMARY KEY ("ID")
)
WITH (
  OIDS=FALSE
);
ALTER TABLE "RANMeter" OWNER TO postgres;
GRANT ALL ON TABLE "RANMeter" TO postgres;

-- Index: visitidndx

-- DROP INDEX visitidndx;

CREATE INDEX visitidndx
  ON "RANMeter"
  USING btree
  ("ID");

-- Index: visitmeterndx

-- DROP INDEX visitmeterndx;

CREATE INDEX visitmeterndx
  ON "RANMeter"
  USING btree
  ("MeterID");

-- Index: visitpalletndx

-- DROP INDEX visitpalletndx;

CREATE INDEX visitpalletndx
  ON "RANMeter"
  USING btree
  ("PalletID");

-- Index: visitrmandx

-- DROP INDEX visitrmandx;

CREATE INDEX visitrmandx
  ON "RANMeter"
  USING btree
  ("RANID");

