DROP TABLE IF EXISTS cond.users;

CREATE TABLE cond.users (
    id serial PRIMARY KEY,
    username text NOT NULL UNIQUE,
    email text NOT NULL UNIQUE,
    bio text,
    image_url text,
    password_hash text NOT NULL
);

ALTER TABLE cond.users OWNER TO conduit_all;

INSERT INTO cond.users (id, username, email, bio, image_url, password_hash)
    VALUES (1, 'uliSchuster', 'ulrich.schuster@koing.de', 'Nothing of importance here', 'https://koing.de/expertise', 'e2a39ee88bb085271cdc0a6a91481b597dba6bdba4687abe5a7eda36491d135a');

INSERT INTO cond.users (id, username, email, bio, image_url, password_hash)
    VALUES (2, 'tomTester', 'tom@test.de', 'Tom is testing', 'http://test.de/test.jpg', 'd6cdd71a6ec4ca043223144ebd1420ca84b54c2bdda050240828692854fa7a4f');

INSERT INTO cond.users (id, username, email, bio, password_hash)
    VALUES (3, 'archie', 'archi.text@architecture.com', 'Invent stuff', '8e5958d9fa47827e62b5d000ff4f5d68445d78aa76c7f70cb9562c1c15e67d91');
