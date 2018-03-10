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

