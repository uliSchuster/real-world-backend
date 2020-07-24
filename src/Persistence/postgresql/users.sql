drop table if exists cond.users;

create table cond.users (
   id serial primary key
  ,username text not null unique
  ,email text not null unique
  ,bio text
  ,image_url text
  , password_hash text not null
  , salt text not null
);

alter table cond.users
  owner to conduit_all;

insert into cond.users (id, username, email, bio, image_url, password_hash, salt) values (1, 'uliSchuster', 'ulrich.schuster@koing.de', 'Nothing of importance here', 'https://koing.de/expertise', 'e2a39ee88bb085271cdc0a6a91481b597dba6bdba4687abe5a7eda36491d135a', '12345678');
insert into cond.users (id, username, email, bio, image_url, password_hash, salt) values (2, 'tomTester', 'tom@test.de', 'Tom is testing', 'http://test.de/test.jpg', 'd6cdd71a6ec4ca043223144ebd1420ca84b54c2bdda050240828692854fa7a4f', '12345678');
insert into cond.users (id, username, email, bio, password_hash, salt) values (3, 'archie', 'archi.text@architecture.com', 'Invent stuff', '8e5958d9fa47827e62b5d000ff4f5d68445d78aa76c7f70cb9562c1c15e67d91', '12345678');
