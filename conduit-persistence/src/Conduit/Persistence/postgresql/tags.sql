DROP TABLE IF EXISTS cond.tags;

CREATE TABLE cond.tags (
    id serial PRIMARY KEY,
    tagname text NOT NULL UNIQUE
);

ALTER TABLE cond.tags OWNER TO conduit_all;

INSERT INTO cond.tags (id, tagname)
    VALUES (1, 'Start');

INSERT INTO cond.tags (id, tagname)
    VALUES (2, 'Test');

INSERT INTO cond.tags (id, tagname)
    VALUES (3, 'Architecture');
