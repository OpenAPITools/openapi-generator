--
-- OAuth2 framework tables for PostgreSQL
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
--

--
-- DROP TABLES 
-- (remove comment prefix to start using DROP commands)
--
-- DROP TABLE IF EXISTS oauth_clients;
-- DROP TABLE IF EXISTS oauth_access_tokens;
-- DROP TABLE IF EXISTS oauth_authorization_codes;
-- DROP TABLE IF EXISTS oauth_refresh_tokens;
-- DROP TABLE IF EXISTS oauth_users;
-- DROP TABLE IF EXISTS oauth_scopes;
-- DROP TABLE IF EXISTS oauth_jwt;
-- DROP TABLE IF EXISTS oauth_jti;
-- DROP TABLE IF EXISTS oauth_public_keys;


--
-- Table oauth_clients
--
CREATE TABLE IF NOT EXISTS oauth_clients (
    client_id            VARCHAR(80)    NOT NULL    PRIMARY KEY,
    client_secret        VARCHAR(80)    DEFAULT NULL,
    redirect_uri         VARCHAR(2000)  DEFAULT NULL,
    grant_types          VARCHAR(80)    DEFAULT NULL,
    scope                VARCHAR(4000)  DEFAULT NULL,
    user_id              VARCHAR(80)    DEFAULT NULL
);

--
-- Table oauth_access_tokens
--
CREATE TABLE IF NOT EXISTS oauth_access_tokens (
    access_token         VARCHAR(40)    NOT NULL    PRIMARY KEY,
    client_id            VARCHAR(80)    DEFAULT NULL,
    user_id              VARCHAR(80)    DEFAULT NULL,
    expires              TIMESTAMP      NOT NULL,
    scope                VARCHAR(4000)  DEFAULT NULL
);

--
-- Table oauth_authorization_codes
--
CREATE TABLE IF NOT EXISTS oauth_authorization_codes (
    authorization_code  VARCHAR(40)    NOT NULL    PRIMARY KEY,
    client_id           VARCHAR(80)    DEFAULT NULL,
    user_id             VARCHAR(80)    DEFAULT NULL,
    redirect_uri        VARCHAR(2000)  NOT NULL,
    expires             TIMESTAMP      NOT NULL,
    scope               VARCHAR(4000)  DEFAULT NULL,
    id_token            VARCHAR(1000)  DEFAULT NULL
);

--
-- Table oauth_refresh_tokens
--
CREATE TABLE IF NOT EXISTS oauth_refresh_tokens (
    refresh_token       VARCHAR(40)    NOT NULL    PRIMARY KEY,
    client_id           VARCHAR(80),
    user_id             VARCHAR(80),
    expires             TIMESTAMP      NOT NULL DEFAULT CURRENT_TIMESTAMP,
    scope               VARCHAR(4000)
);

--
-- Table oauth_users
--
CREATE TABLE IF NOT EXISTS oauth_users (
    username            VARCHAR(80)    DEFAULT NULL,
    password            VARCHAR(255)   DEFAULT NULL,
    first_name          VARCHAR(80)    DEFAULT NULL,
    last_name           VARCHAR(80)    DEFAULT NULL,
    email               VARCHAR(2000)  DEFAULT NULL,
    email_verified      SMALLINT       DEFAULT NULL,
    scope               VARCHAR(4000)  DEFAULT NULL
);

--
-- Table oauth_scopes
--
CREATE TABLE IF NOT EXISTS oauth_scopes (
    scope               VARCHAR(80)  NOT NULL    PRIMARY KEY,
    is_default          SMALLINT     DEFAULT NULL
);

--
-- Table oauth_jwt
--
CREATE TABLE IF NOT EXISTS oauth_jwt (
    client_id           VARCHAR(80)    NOT NULL,
    subject             VARCHAR(80)    DEFAULT NULL,
    public_key          VARCHAR(2000)  NOT NULL
);

--
-- Table oauth_jti
--
CREATE TABLE IF NOT EXISTS oauth_jti (
    issuer              VARCHAR(80)    NOT NULL,
    subject             VARCHAR(80)    DEFAULT NULL,
    audience            VARCHAR(80)    DEFAULT NULL,
    expires             TIMESTAMP      NOT NULL,
    jti                 VARCHAR(2000)  NOT NULL
);

--
-- Table oauth_public_keys
--
CREATE TABLE IF NOT EXISTS oauth_public_keys (
    client_id            VARCHAR(80)    DEFAULT NULL,
    public_key           VARCHAR(2000)  DEFAULT NULL,
    private_key          VARCHAR(2000)  DEFAULT NULL,
    encryption_algorithm VARCHAR(100)   DEFAULT 'RS256'
);

