create table users(
  username varchar(20) primary key,
  password varchar(20) not null,
  tries int not null,
  corrects int not null
);

