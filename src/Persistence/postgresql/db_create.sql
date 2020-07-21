-- Database: conduit
-- DROP DATABASE conduit;

CREATE DATABASE conduit
    WITH OWNER = postgres
    ENCODING = 'UTF8'
        LC_COLLATE = 'C'
        LC_CTYPE = 'C'
        TABLESPACE = pg_default
    CONNECTION LIMIT = - 1;

COMMENT ON DATABASE conduit IS 'Database for the Real World Haskell blogging application.';

