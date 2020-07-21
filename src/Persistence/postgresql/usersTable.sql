create table (if not exists) users(
   id serial primary key
  ,username text not null unique
  ,email text not null
  ,bio text
  ,image_url text
);
