DROP TABLE IF EXISTS cond.articles_tags;

CREATE TABLE cond.articles_tags (
    article_fk integer NOT NULL,
    tag_fk integer NOT NULL,
    PRIMARY KEY (article_fk, tag_fk),
    FOREIGN KEY (article_fk) REFERENCES cond.articles (id) ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (tag_fk) REFERENCES cond.tags (id) ON UPDATE CASCADE ON DELETE CASCADE
);

ALTER TABLE cond.articles_tags OWNER TO conduit_all;

INSERT INTO cond.articles_tags (article_fk, tag_fk)
    VALUES (1, 1);

INSERT INTO cond.articles_tags (article_fk, tag_fk)
    VALUES (2, 1);

INSERT INTO cond.articles_tags (article_fk, tag_fk)
    VALUES (2, 2);

INSERT INTO cond.articles_tags (article_fk, tag_fk)
    VALUES (3, 2);

INSERT INTO cond.articles_tags (article_fk, tag_fk)
    VALUES (3, 3);

