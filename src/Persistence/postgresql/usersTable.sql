drop table if exists users;

create table users (
   id serial primary key
  ,username text not null unique
  ,email text not null
  ,bio text
  ,image_url text
);

insert into users (username, email, bio, image_url) values ('uliSchuster', 'ulrich.schuster@koing.de', 'Nothing of importance here', 'https://koing.de/expertise');
insert into users (username, email, bio, image_url) values ('tomTester', 'tom@test.de', 'Tom is testing', 'http://test.de/test.jpg');
insert into users (username, email, bio) values ('archie', 'archi.text@architecture.com', 'Invent stuff');
