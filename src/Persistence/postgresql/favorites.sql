DROP TABLE IF EXISTS cond.favorites;

CREATE TABLE cond.favorites (
    reader_fk integer NOT NULL,
    favorite_fk integer NOT NULL,
    PRIMARY KEY (reader_fk, favorite_fk),
    FOREIGN KEY (reader_fk) REFERENCES cond.users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (favorite_fk) REFERENCES cond.articles (id) ON UPDATE CASCADE ON DELETE CASCADE
);

ALTER TABLE cond.favorites OWNER TO conduit_all;

INSERT INTO cond.favorites (reader_fk, favorite_fk)
    VALUES (1, 2);

INSERT INTO cond.favorites (reader_fk, favorite_fk)
    VALUES (3, 1);
