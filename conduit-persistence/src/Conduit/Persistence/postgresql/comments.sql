DROP TABLE IF EXISTS cond.comments;

CREATE TABLE cond.comments (
    id serial PRIMARY KEY,
    article_fk integer NOT NULL,
    author_fk integer NOT NULL,
    body text NOT NULL,
    created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UNIQUE (article_fk, author_fk, body),
    FOREIGN KEY (article_fk) REFERENCES cond.articles (id) ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (author_fk) REFERENCES cond.users (id) ON UPDATE CASCADE ON DELETE CASCADE
);

ALTER TABLE cond.comments OWNER TO conduit_all;

INSERT INTO cond.comments (id, article_fk, author_fk, body)
    VALUES (1, 1, 2, 'This is a test comment');

INSERT INTO cond.comments (id, article_fk, author_fk, body)
    VALUES (2, 1, 3, 'An architectural comment');
