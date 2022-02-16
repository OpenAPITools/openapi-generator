

-- --------------------------------------------------------------------------
-- Table structure for table `ApiResponse` generated from model 'apiResponse'
-- Describes the result of uploading an image resource
--

CREATE TABLE IF NOT EXISTS `ApiResponse` (
  `code` int,
  `type` text,
  `message` text
);  /*Describes the result of uploading an image resource*/


-- --------------------------------------------------------------------------
-- Table structure for table `Category` generated from model 'category'
-- A category for a pet
--

CREATE TABLE IF NOT EXISTS `Category` (
  `id` long PRIMARY KEY AUTOINCREMENT,
  `name` text
);  /*A category for a pet*/


-- --------------------------------------------------------------------------
-- Table structure for table `Order` generated from model 'order'
-- An order for a pets from the pet store
--

CREATE TABLE IF NOT EXISTS `Order` (
  `id` long PRIMARY KEY AUTOINCREMENT,
  `petId` long,
  `quantity` int,
  `shipDate` datetime,
  `status` text /*Order Status*/,
  `complete` boolean
);  /*An order for a pets from the pet store*/


-- --------------------------------------------------------------------------
-- Table structure for table `Pet` generated from model 'pet'
-- A pet for sale in the pet store
--

CREATE TABLE IF NOT EXISTS `Pet` (
  `name` text NOT NULL,
  `id` long PRIMARY KEY AUTOINCREMENT,
  `category` long,
  `status` text /*pet status in the store*/
);  /*A pet for sale in the pet store*/

-- --------------------------------------------------------------------------
-- Table structure for table `PetPhotoUrls` generated from model 'PetPhotoUrls'

CREATE TABLE IF NOT EXISTS `PetPhotoUrls` (
  `pet` long NOT NULL
  `photoUrls` text NOT NULL
);

-- --------------------------------------------------------------------------
-- Table structure for table `PetTag` generated from model 'PetTag'

CREATE TABLE IF NOT EXISTS `PetTag` (
  `pet` long NOT NULL
  `tag` long NOT NULL
);


-- --------------------------------------------------------------------------
-- Table structure for table `Tag` generated from model 'tag'
-- A tag for a pet
--

CREATE TABLE IF NOT EXISTS `Tag` (
  `id` long PRIMARY KEY AUTOINCREMENT,
  `name` text
);  /*A tag for a pet*/


-- --------------------------------------------------------------------------
-- Table structure for table `User` generated from model 'user'
-- A User who is purchasing from the pet store
--

CREATE TABLE IF NOT EXISTS `User` (
  `id` long PRIMARY KEY AUTOINCREMENT,
  `username` text,
  `firstName` text,
  `lastName` text,
  `email` text,
  `password` text,
  `phone` text,
  `userStatus` int /*User Status*/
);  /*A User who is purchasing from the pet store*/


--
-- OAuth2 framework tables
-- Thanks to https://github.com/dsquier/oauth2-server-php-mysql repo
--

--
-- Table structure for table `oauth_clients`
--
CREATE TABLE IF NOT EXISTS `oauth_clients` (
  `client_id`            VARCHAR(80)    NOT NULL,
  `client_secret`        VARCHAR(80)    DEFAULT NULL,
  `redirect_uri`         VARCHAR(2000)  DEFAULT NULL,
  `grant_types`          VARCHAR(80)    DEFAULT NULL,
  `scope`                VARCHAR(4000)  DEFAULT NULL,
  `user_id`              VARCHAR(80)    DEFAULT NULL,
  PRIMARY KEY (`client_id`)
);

--
-- Table structure for table `oauth_access_tokens`
--
CREATE TABLE IF NOT EXISTS `oauth_access_tokens` (
  `access_token`         VARCHAR(40)    NOT NULL,
  `client_id`            VARCHAR(80)    DEFAULT NULL,
  `user_id`              VARCHAR(80)    DEFAULT NULL,
  `expires`              TIMESTAMP      NOT NULL,
  `scope`                VARCHAR(4000)  DEFAULT NULL,
  PRIMARY KEY (`access_token`)
);

--
-- Table structure for table `oauth_authorization_codes`
--
CREATE TABLE IF NOT EXISTS `oauth_authorization_codes` (
  `authorization_code`  VARCHAR(40)    NOT NULL,
  `client_id`           VARCHAR(80)    DEFAULT NULL,
  `user_id`             VARCHAR(80)    DEFAULT NULL,
  `redirect_uri`        VARCHAR(2000)  NOT NULL,
  `expires`             TIMESTAMP      NOT NULL,
  `scope`               VARCHAR(4000)  DEFAULT NULL,
  `id_token`            VARCHAR(1000)  DEFAULT NULL,
  PRIMARY KEY (`authorization_code`)
);

--
-- Table structure for table `oauth_refresh_tokens`
--
CREATE TABLE IF NOT EXISTS `oauth_refresh_tokens` (
  `refresh_token`       VARCHAR(40)    NOT NULL,
  `client_id`           VARCHAR(80)    DEFAULT NULL,
  `user_id`             VARCHAR(80)    DEFAULT NULL,
  `expires`             TIMESTAMP      DEFAULT CURRENT_TIMESTAMP,
  `scope`               VARCHAR(4000)  DEFAULT NULL,
  PRIMARY KEY (`refresh_token`)
);

--
-- Table structure for table `oauth_users`
--
CREATE TABLE IF NOT EXISTS `oauth_users` (
  `username`            VARCHAR(80)    DEFAULT NULL,
  `password`            VARCHAR(255)   DEFAULT NULL,
  `first_name`          VARCHAR(80)    DEFAULT NULL,
  `last_name`           VARCHAR(80)    DEFAULT NULL,
  `email`               VARCHAR(2000)  DEFAULT NULL,
  `email_verified`      TINYINT(1)     DEFAULT NULL,
  `scope`               VARCHAR(4000)  DEFAULT NULL
);

--
-- Table structure for table `oauth_scopes`
--
CREATE TABLE IF NOT EXISTS `oauth_scopes` (
  `scope`               VARCHAR(80)  NOT NULL,
  `is_default`          TINYINT(1)   DEFAULT NULL,
  PRIMARY KEY (`scope`)
);

--
-- Table structure for table `oauth_jwt`
--
CREATE TABLE IF NOT EXISTS `oauth_jwt` (
  `client_id`           VARCHAR(80)    NOT NULL,
  `subject`             VARCHAR(80)    DEFAULT NULL,
  `public_key`          VARCHAR(2000)  NOT NULL
);

--
-- Table structure for table `oauth_jti`
--
CREATE TABLE IF NOT EXISTS `oauth_jti` (
  `issuer`              VARCHAR(80)    NOT NULL,
  `subject`             VARCHAR(80)    DEFAULT NULL,
  `audience`            VARCHAR(80)    DEFAULT NULL,
  `expires`             TIMESTAMP      NOT NULL,
  `jti`                 VARCHAR(2000)  NOT NULL
);

--
-- Table structure for table `oauth_public_keys`
--
CREATE TABLE IF NOT EXISTS `oauth_public_keys` (
  `client_id`            VARCHAR(80)    DEFAULT NULL,
  `public_key`           VARCHAR(2000)  DEFAULT NULL,
  `private_key`          VARCHAR(2000)  DEFAULT NULL,
  `encryption_algorithm` VARCHAR(100)   DEFAULT 'RS256'
);

--
-- Table structure for table `_db_version`
--
CREATE TABLE IF NOT EXISTS `_db_version` (
  `version`    LONG    DEFAULT 1
);
