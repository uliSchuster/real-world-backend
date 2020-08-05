DROP TABLE IF EXISTS cond.follows;

CREATE TABLE cond.follows (
    follower_fk integer NOT NULL,
    followee_fk integer NOT NULL,
    PRIMARY KEY (follower_fk, followee_fk),
    FOREIGN KEY (follower_fk) REFERENCES cond.users (id) ON UPDATE CASCADE ON DELETE CASCADE,
    FOREIGN KEY (followee_fk) REFERENCES cond.users (id) ON UPDATE CASCADE ON DELETE CASCADE
);

ALTER TABLE cond.follows OWNER TO conduit_all;

INSERT INTO cond.follows (follower_fk, followee_fk)
    VALUES (1, 2);

INSERT INTO cond.follows (follower_fk, followee_fk)
    VALUES (3, 2);
