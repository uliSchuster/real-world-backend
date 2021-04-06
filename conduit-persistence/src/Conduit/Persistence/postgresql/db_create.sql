CREATE USER cond WITH PASSWORD 'conduit';

-- Database: conduit
DROP DATABASE IF EXISTS conduit;

CREATE DATABASE conduit WITH OWNER = cond ENCODING = 'UTF8' LC_COLLATE = 'C' LC_CTYPE = 'C' TABLESPACE = pg_default CONNECTION LIMIT = - 1 TEMPLATE template0;

COMMENT ON DATABASE conduit IS 'Database for the Real World Haskell blogging application.';

-- For now, do not secure access = one user has all privileges.
-- However, provide a roles structure that later on would allow to
-- have separate users for reading, writing and administering the DB.
-- See: https://serverfault.com/questions/483998/postgres-roles-best-practice-implementation
-- and http://www.jancarloviray.com/blog/postgres-quick-start-and-best-practices/
CREATE ROLE conduit_all LOGIN INHERIT;

GRANT ALL PRIVILEGES ON DATABASE conduit TO conduit_all;

GRANT conduit_all TO cond;

CREATE SCHEMA cond AUTHORIZATION cond;

