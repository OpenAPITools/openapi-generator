--
-- Schema objects for PostgreSQL
-- "OpenAPI Petstore"
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
--

--
-- DROP OBJECTS
-- (remove comment prefix to start using DROP commands)
--
-- TABLES
--
-- DROP TABLE IF EXISTS ApiResponse;
-- DROP TABLE IF EXISTS Category;
-- DROP TABLE IF EXISTS "Order";
-- DROP TABLE IF EXISTS Pet;
-- DROP TABLE IF EXISTS Tag;
-- DROP TABLE IF EXISTS "User";

--
-- TYPES
--
-- DROP TYPE IF EXISTS Order_status;
-- DROP TYPE IF EXISTS Pet_status;


--
-- CREATE OBJECTS
--
-- TYPES
--
CREATE TYPE Order_status AS ENUM('placed', 'approved', 'delivered');
CREATE TYPE Pet_status AS ENUM('available', 'pending', 'sold');

--
-- TABLES
--
--
-- Table 'ApiResponse' generated from model 'ApiResponse'
-- Describes the result of uploading an image resource
--
CREATE TABLE IF NOT EXISTS ApiResponse (
    code INT DEFAULT NULL,
    "type" VARCHAR(255) DEFAULT NULL,
    message VARCHAR(255) DEFAULT NULL
);
COMMENT ON TABLE ApiResponse IS 'Describes the result of uploading an image resource';

--
-- Table 'Category' generated from model 'Category'
-- A category for a pet
--
CREATE TABLE IF NOT EXISTS Category (
    id BIGINT DEFAULT NULL,
    "name" VARCHAR(255) DEFAULT NULL
);
COMMENT ON TABLE Category IS 'A category for a pet';

--
-- Table 'Order' generated from model 'Order'
-- An order for a pets from the pet store
--
CREATE TABLE IF NOT EXISTS "Order" (
    id BIGINT DEFAULT NULL,
    petId BIGINT DEFAULT NULL,
    quantity INT DEFAULT NULL,
    shipDate TIMESTAMP DEFAULT NULL,
    status Order_status DEFAULT NULL,
    complete BOOLEAN DEFAULT 'false'
);
COMMENT ON TABLE "Order" IS 'An order for a pets from the pet store';
COMMENT ON COLUMN "Order".status IS 'Order Status';

--
-- Table 'Pet' generated from model 'Pet'
-- A pet for sale in the pet store
--
CREATE TABLE IF NOT EXISTS Pet (
    id BIGINT DEFAULT NULL,
    category TEXT DEFAULT NULL,
    "name" VARCHAR(255) NOT NULL,
    photoUrls JSON NOT NULL,
    tags JSON DEFAULT NULL,
    status Pet_status DEFAULT NULL
);
COMMENT ON TABLE Pet IS 'A pet for sale in the pet store';
COMMENT ON COLUMN Pet.status IS 'pet status in the store';

--
-- Table 'Tag' generated from model 'Tag'
-- A tag for a pet
--
CREATE TABLE IF NOT EXISTS Tag (
    id BIGINT DEFAULT NULL,
    "name" VARCHAR(255) DEFAULT NULL
);
COMMENT ON TABLE Tag IS 'A tag for a pet';

--
-- Table 'User' generated from model 'User'
-- A User who is purchasing from the pet store
--
CREATE TABLE IF NOT EXISTS "User" (
    id BIGINT DEFAULT NULL,
    username VARCHAR(255) DEFAULT NULL,
    firstName VARCHAR(255) DEFAULT NULL,
    lastName VARCHAR(255) DEFAULT NULL,
    email VARCHAR(255) DEFAULT NULL,
    "password" VARCHAR(255) DEFAULT NULL,
    phone VARCHAR(255) DEFAULT NULL,
    userStatus INT DEFAULT NULL
);
COMMENT ON TABLE "User" IS 'A User who is purchasing from the pet store';
COMMENT ON COLUMN "User".userStatus IS 'User Status';

