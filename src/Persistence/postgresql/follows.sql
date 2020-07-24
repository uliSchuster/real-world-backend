drop table if exists cond.follows;

create table cond.follows (
     follower_fk integer not null
    ,followee_fk integer not null
    ,primary key (follower_fk, followee_fk)
    ,foreign key (follower_fk) references cond.users (id)
        on update cascade
        on delete cascade
    ,foreign key (followee_fk) references cond.users (id)
        on update cascade
        on delete cascade
);

alter table cond.follows
  owner to conduit_all;

insert into cond.follows (follower_fk, followee_fk) values(1, 2);
insert into cond.follows (follower_fk, followee_fk) values(3, 2);
