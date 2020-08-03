drop table if exists cond.articles_tags;

create table cond.articles_tags (
     article_fk integer not null
    ,tag_fk integer not null
    ,primary key (article_fk, tag_fk)
    ,foreign key (article_fk) references cond.articles (id)
        on update cascade
        on delete cascade
    ,foreign key (tag_fk) references cond.tags (id)
        on update cascade
        on delete cascade
);

alter table cond.articles_tags
  owner to conduit_all;

insert into cond.articles_tags (article_fk, tag_fk) values(1, 1);
insert into cond.articles_tags (article_fk, tag_fk) values(2, 1);
insert into cond.articles_tags (article_fk, tag_fk) values(2, 2);
insert into cond.articles_tags (article_fk, tag_fk) values(3, 2);
insert into cond.articles_tags (article_fk, tag_fk) values(3, 3);