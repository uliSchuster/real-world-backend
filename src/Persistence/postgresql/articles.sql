drop table if exists cond.articles;

create table cond.articles (
        id serial primary key
       ,author_fk integer not null
       ,title text not null
       ,description text not null
       ,body text not null
       ,created_at timestamptz not null default current_timestamp
       ,updated_at timestamptz not null default current_timestamp
       ,unique (author_fk, title)
       ,foreign key (author_fk)
            references cond.users (id)
            on update cascade
            on delete cascade
);

alter table cond.articles
  owner to conduit_all;

insert into cond.articles (id, author_fk, title, description, body) values(1, 1, 'My first article', 'This is a test article', 'My first article does not have any content.');
insert into cond.articles (id, author_fk, title, description, body) values(2, 2, 'Test article', 'Test', 'This article is a test.');
insert into cond.articles (id, author_fk, title, description, body) values(3, 3, 'All about architecture', 'Archie the architect', 'This is an article about architecture.');