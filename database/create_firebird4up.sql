create table firebird4datatypes (
  id integer not null,
  time_with_time_zone TIME WITH TIME ZONE,
  timestamp_with_time_zone TIMESTAMP WITH TIME ZONE,
  decfloat16 DECFLOAT(16),
  decfloat34 DECFLOAT(34),
  float30 FLOAT(30),
  constraint pk_firebird4datatypes primary key (id)
);
