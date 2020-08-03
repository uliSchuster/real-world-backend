DROP TABLE IF EXISTS cond.articles;

CREATE TABLE cond.articles (
    id serial PRIMARY KEY,
    author_fk integer NOT NULL,
    title text NOT NULL,
    description text NOT NULL,
    body text NOT NULL,
    created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UNIQUE (author_fk, title),
    FOREIGN KEY (author_fk) REFERENCES cond.users (id) ON UPDATE CASCADE ON DELETE CASCADE
);

ALTER TABLE cond.articles OWNER TO conduit_all;

INSERT INTO cond.articles (id, author_fk, title, description, body)
    VALUES (1, 1, 'My first article', 'This is a test article', 'My first article does not have any content.');

INSERT INTO cond.articles (id, author_fk, title, description, body)
    VALUES (2, 2, 'Test article', 'Test', 'This article is a test.');

INSERT INTO cond.articles (id, author_fk, title, description, body)
    VALUES (3, 3, 'All about architecture', 'Archie the architect', 'This is an article about architecture.');
