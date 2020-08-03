drop table if exists cond.favorites;

create table cond.favorites (
     reader_fk integer not null
    ,favorite_fk integer not null 
    ,primary key (reader_fk, favorite_fk)
    ,foreign key (reader_fk) references cond.users (id)
        on update cascade
        on delete cascade
    ,foreign key (favorite_fk) references cond.articles (id)
        on update cascade
        on delete cascade
);

alter table cond.favorites
  owner to conduit_all;


insert into cond.favorites (reader_fk, favorite_fk) values(1, 2);
insert into cond.favorites (reader_fk, favorite_fk) values(3, 1);
