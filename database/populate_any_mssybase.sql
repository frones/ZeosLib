/*==============================================================*/
/* Database name:  zeoslib                                      */
/* Created on:     28.12.2002 20:27:07                          */
/*==============================================================*/

INSERT INTO department VALUES (3,'Delivery agency','UKR','Donetsk Artema st. 113')
INSERT INTO department VALUES (2,'Container agency','USA','Krasnodar Komsomolskaya st. 17') 
INSERT INTO department VALUES (1,'Line agency','RUS','Novorossiysk Lenina st. 2') 
go

INSERT INTO equipment VALUES (1,'Volvo',1,15000.0000,'19980304',NULL)
INSERT INTO equipment VALUES (2,'Laboratoy',10,40000.0000,'20011007',NULL)
INSERT INTO equipment VALUES (3,'Computer',7,900.0000,'19990903',NULL)
INSERT INTO equipment VALUES (4,'Radiostation',19,400.0000,'20000708',NULL)
go

INSERT INTO equipment2 VALUES (1,1)
INSERT INTO equipment2 VALUES (1,2)
INSERT INTO equipment2 VALUES (1,4)
INSERT INTO equipment2 VALUES (2,1)
INSERT INTO equipment2 VALUES (2,3)
go

INSERT INTO people VALUES (1,1,'Vasia Pupkin','1899-12-30T09:00:00','1899-12-30T18:00:00',NULL,NULL,0)
INSERT INTO people VALUES (2,2,'Andy Karto','1899-12-30T08:30:00','1899-12-30T17:30:00',NULL,NULL,0)
INSERT INTO people VALUES (3,1,'Kristen Sato','1899-12-30T09:00:00','1899-12-30T18:00:00',NULL,NULL,0)
INSERT INTO people VALUES (4,2,'Aleksey Petrov','1899-12-30T08:30:00','1899-12-30T17:30:00',NULL,NULL,1)
INSERT INTO people VALUES (5,3,'Yan Pater','1899-12-30T08:00:00','1899-12-30T17:00:00',NULL,NULL,1)
go

INSERT INTO cargo VALUES (1,2,'Grain',1,'2002-12-20T02:00:00','2002-12-20T02:00:00',5000,NULL,NULL,1769.4300,NULL)
INSERT INTO cargo VALUES (2,1,'Paper',2,'2002-12-19T14:00:00','2002-12-23T00:00:00',1000,10,10,986.4700,convert(binary(10),'aaa'))
INSERT INTO cargo VALUES (3,1,'Wool',0,'2002-12-20T18:00:00',NULL,400,7,4,643.1100,NULL)
INSERT INTO cargo VALUES (4,2,'Suagr',1,'2002-12-21T10:20:00','2002-12-26T00:00:00',2034,NULL,NULL,1964.8700,NULL)
go
INSERT INTO bcd_values(id, curr18_4, curr15_2, curr10_4, curr4_4, bigd18_1, bigd18_5, bigd12_10, bigd18_18) VALUES 
  (1, 12345678901234.5678, 1234567890123.45, 123456.7890, 0.1234, 12345678901234567.8, 1234567890123.45678, 12.3456789012, 0.123456789012345678)
go