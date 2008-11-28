create table users(
  username varchar(50) primary key,
  password varchar(50) not null,
  tries int not null,
  corrects int not null
);

create table sessions(
  session_id varchar(50) primary key,
  authenticated_user varchar(50) not null
);
