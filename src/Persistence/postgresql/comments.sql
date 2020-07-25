drop table if exists cond.comments;

create table cond.comments (
     id serial primary key
    ,article_fk integer not null
    ,author_fk integer not null
    ,body text not null
    ,created_at timestamptz not null default current_timestamp
    ,updated_at timestamptz not null default current_timestamp
    ,unique (article_fk, author_fk, body)
    ,foreign key (article_fk) references cond.articles (id)
        on update cascade
        on delete cascade
    ,foreign key (author_fk) references cond.users (id)
        on update cascade
        on delete cascade
);

alter table cond.comments
  owner to conduit_all;

insert into cond.comments (id, article_fk, author_fk, body) values(1, 1, 2, 'This is a test comment');
insert into cond.comments (id, article_fk, author_fk, body) values(2, 1, 3, 'An architectural comment');