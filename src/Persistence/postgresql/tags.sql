drop table if exists cond.tags;

create table cond.tags (
     id serial primary key
    ,tagname text not null unique
);

alter table cond.tags
  owner to conduit_all;

insert into cond.tags (id, tagname) values(1, 'Start');
insert into cond.tags (id, tagname) values(2, 'Test');
insert into cond.tags (id, tagname) values(3, 'Architecture');
