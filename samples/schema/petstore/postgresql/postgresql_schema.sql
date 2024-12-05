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
-- DROP TABLE IF EXISTS api_response;
-- DROP TABLE IF EXISTS category;
-- DROP TABLE IF EXISTS "order";
-- DROP TABLE IF EXISTS pet;
-- DROP TABLE IF EXISTS tag;
-- DROP TABLE IF EXISTS "user";

--
-- TYPES
--
-- DROP TYPE IF EXISTS order_status;
-- DROP TYPE IF EXISTS pet_status;


--
-- CREATE OBJECTS
--
-- TYPES
--
CREATE TYPE order_status AS ENUM('placed', 'approved', 'delivered');
CREATE TYPE pet_status AS ENUM('available', 'pending', 'sold');

--
-- TABLES
--
--
-- Table 'api_response' generated from model 'ApiResponse'
-- Describes the result of uploading an image resource
--
CREATE TABLE IF NOT EXISTS api_response (
    code INTEGER DEFAULT NULL,
    "type" TEXT DEFAULT NULL,
    message TEXT DEFAULT NULL
);
COMMENT ON TABLE api_response IS 'Describes the result of uploading an image resource. Original model name - ApiResponse.';

--
-- Table 'category' generated from model 'Category'
-- A category for a pet
--
CREATE TABLE IF NOT EXISTS category (
    "id" BIGINT DEFAULT NULL,
    "name" TEXT DEFAULT NULL
);
COMMENT ON TABLE category IS 'A category for a pet. Original model name - Category.';

--
-- Table 'order' generated from model 'Order'
-- An order for a pets from the pet store
--
CREATE TABLE IF NOT EXISTS "order" (
    "id" BIGINT DEFAULT NULL,
    pet_id BIGINT DEFAULT NULL,
    quantity INTEGER DEFAULT NULL,
    ship_date TIMESTAMP DEFAULT NULL,
    status order_status DEFAULT NULL,
    complete BOOLEAN DEFAULT 'false'
);
COMMENT ON TABLE "order" IS 'An order for a pets from the pet store. Original model name - Order.';
COMMENT ON COLUMN "order".pet_id IS 'Original param name - petId.';
COMMENT ON COLUMN "order".ship_date IS 'Original param name - shipDate.';
COMMENT ON COLUMN "order".status IS 'Order Status';

--
-- Table 'pet' generated from model 'Pet'
-- A pet for sale in the pet store
--
CREATE TABLE IF NOT EXISTS pet (
    "id" BIGINT DEFAULT NULL,
    category TEXT DEFAULT NULL,
    "name" TEXT NOT NULL,
    photo_urls JSON NOT NULL,
    tags JSON DEFAULT NULL,
    status pet_status DEFAULT NULL
);
COMMENT ON TABLE pet IS 'A pet for sale in the pet store. Original model name - Pet.';
COMMENT ON COLUMN pet.photo_urls IS 'Original param name - photoUrls.';
COMMENT ON COLUMN pet.status IS 'pet status in the store';

--
-- Table 'tag' generated from model 'Tag'
-- A tag for a pet
--
CREATE TABLE IF NOT EXISTS tag (
    "id" BIGINT DEFAULT NULL,
    "name" TEXT DEFAULT NULL
);
COMMENT ON TABLE tag IS 'A tag for a pet. Original model name - Tag.';

--
-- Table 'user' generated from model 'User'
-- A User who is purchasing from the pet store
--
CREATE TABLE IF NOT EXISTS "user" (
    "id" BIGINT DEFAULT NULL,
    username TEXT DEFAULT NULL,
    first_name TEXT DEFAULT NULL,
    last_name TEXT DEFAULT NULL,
    email TEXT DEFAULT NULL,
    "password" TEXT DEFAULT NULL,
    phone TEXT DEFAULT NULL,
    user_status INTEGER DEFAULT NULL
);
COMMENT ON TABLE "user" IS 'A User who is purchasing from the pet store. Original model name - User.';
COMMENT ON COLUMN "user".first_name IS 'Original param name - firstName.';
COMMENT ON COLUMN "user".last_name IS 'Original param name - lastName.';
COMMENT ON COLUMN "user".user_status IS 'User Status. Original param name - userStatus.';

