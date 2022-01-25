/* SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO"; */
/* SET AUTOCOMMIT = 0; */
/* START TRANSACTION; */
/* SET time_zone = "+00:00"; */

-- --------------------------------------------------------

--
-- Table structure for table `200_response` generated from model '200Underscoreresponse'
-- Model for testing model name starting with number
--

CREATE TABLE IF NOT EXISTS `200_response` (
  `name` INT DEFAULT NULL,
  `class` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='Model for testing model name starting with number';

--
-- Table structure for table `AdditionalPropertiesClass` generated from model 'AdditionalPropertiesClass'
--

CREATE TABLE IF NOT EXISTS `AdditionalPropertiesClass` (
  `map_property` JSON DEFAULT NULL,
  `map_of_map_property` JSON DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Animal` generated from model 'Animal'
--

CREATE TABLE IF NOT EXISTS `Animal` (
  `className` TEXT NOT NULL,
  `color` TEXT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `ApiResponse` generated from model 'ApiResponse'
--

CREATE TABLE IF NOT EXISTS `ApiResponse` (
  `code` INT DEFAULT NULL,
  `type` TEXT DEFAULT NULL,
  `message` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `ArrayOfArrayOfNumberOnly` generated from model 'ArrayOfArrayOfNumberOnly'
--

CREATE TABLE IF NOT EXISTS `ArrayOfArrayOfNumberOnly` (
  `ArrayArrayNumber` JSON DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `ArrayOfNumberOnly` generated from model 'ArrayOfNumberOnly'
--

CREATE TABLE IF NOT EXISTS `ArrayOfNumberOnly` (
  `ArrayNumber` JSON DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `ArrayTest` generated from model 'ArrayTest'
--

CREATE TABLE IF NOT EXISTS `ArrayTest` (
  `array_of_string` JSON DEFAULT NULL,
  `array_array_of_integer` JSON DEFAULT NULL,
  `array_array_of_model` JSON DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Capitalization` generated from model 'Capitalization'
--

CREATE TABLE IF NOT EXISTS `Capitalization` (
  `smallCamel` TEXT DEFAULT NULL,
  `CapitalCamel` TEXT DEFAULT NULL,
  `small_Snake` TEXT DEFAULT NULL,
  `Capital_Snake` TEXT DEFAULT NULL,
  `SCA_ETH_Flow_Points` TEXT DEFAULT NULL,
  `ATT_NAME` TEXT DEFAULT NULL COMMENT 'Name of the pet '
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Cat` generated from model 'Cat'
--

CREATE TABLE IF NOT EXISTS `Cat` (
  `className` TEXT NOT NULL,
  `color` TEXT,
  `declawed` TINYINT(1) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Cat_allOf` generated from model 'CatUnderscoreallOf'
--

CREATE TABLE IF NOT EXISTS `Cat_allOf` (
  `declawed` TINYINT(1) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Category` generated from model 'Category'
--

CREATE TABLE IF NOT EXISTS `Category` (
  `id` BIGINT DEFAULT NULL,
  `name` TEXT NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `ClassModel` generated from model 'ClassModel'
-- Model for testing model with \&quot;_class\&quot; property
--

CREATE TABLE IF NOT EXISTS `ClassModel` (
  `_class` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='Model for testing model with \&quot;_class\&quot; property';

--
-- Table structure for table `Client` generated from model 'Client'
--

CREATE TABLE IF NOT EXISTS `Client` (
  `client` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `DeprecatedObject` generated from model 'DeprecatedObject'
--

CREATE TABLE IF NOT EXISTS `DeprecatedObject` (
  `name` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Dog` generated from model 'Dog'
--

CREATE TABLE IF NOT EXISTS `Dog` (
  `className` TEXT NOT NULL,
  `color` TEXT,
  `breed` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Dog_allOf` generated from model 'DogUnderscoreallOf'
--

CREATE TABLE IF NOT EXISTS `Dog_allOf` (
  `breed` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `EnumArrays` generated from model 'EnumArrays'
--

CREATE TABLE IF NOT EXISTS `EnumArrays` (
  `just_symbol` ENUM('&gt;&#x3D;', '$') DEFAULT NULL,
  `array_enum` JSON DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Enum_Test` generated from model 'EnumUnderscoreTest'
--

CREATE TABLE IF NOT EXISTS `Enum_Test` (
  `enum_string` ENUM('UPPER', 'lower', '') DEFAULT NULL,
  `enum_string_required` ENUM('UPPER', 'lower', '') NOT NULL,
  `enum_integer` ENUM('1', '-1') DEFAULT NULL,
  `enum_number` ENUM('1.1', '-1.2') DEFAULT NULL,
  `outerEnum` TEXT DEFAULT NULL,
  `outerEnumInteger` TEXT DEFAULT NULL,
  `outerEnumDefaultValue` TEXT DEFAULT NULL,
  `outerEnumIntegerDefaultValue` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `File` generated from model 'File'
-- Must be named &#x60;File&#x60; for test.
--

CREATE TABLE IF NOT EXISTS `File` (
  `sourceURI` TEXT DEFAULT NULL COMMENT 'Test capitalization'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='Must be named &#x60;File&#x60; for test.';

--
-- Table structure for table `FileSchemaTestClass` generated from model 'FileSchemaTestClass'
--

CREATE TABLE IF NOT EXISTS `FileSchemaTestClass` (
  `file` TEXT DEFAULT NULL,
  `files` JSON DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Foo` generated from model 'Foo'
--

CREATE TABLE IF NOT EXISTS `Foo` (
  `bar` TEXT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `format_test` generated from model 'formatUnderscoretest'
--

CREATE TABLE IF NOT EXISTS `format_test` (
  `integer` TINYINT UNSIGNED DEFAULT NULL,
  `int32` TINYINT UNSIGNED DEFAULT NULL,
  `int64` BIGINT DEFAULT NULL,
  `number` DECIMAL(20, 9) UNSIGNED NOT NULL,
  `float` DECIMAL(20, 9) UNSIGNED DEFAULT NULL,
  `double` DECIMAL(20, 9) UNSIGNED DEFAULT NULL,
  `decimal` TEXT DEFAULT NULL,
  `string` TEXT DEFAULT NULL,
  `byte` MEDIUMBLOB NOT NULL,
  `binary` MEDIUMBLOB DEFAULT NULL,
  `date` DATE NOT NULL,
  `dateTime` DATETIME DEFAULT NULL,
  `uuid` TEXT DEFAULT NULL,
  `password` VARCHAR(64) NOT NULL,
  `pattern_with_digits` TEXT DEFAULT NULL COMMENT 'A string that is a 10 digit number. Can have leading zeros.',
  `pattern_with_digits_and_delimiter` TEXT DEFAULT NULL COMMENT 'A string starting with &#39;image_&#39; (case insensitive) and one to three digits following i.e. Image_01.'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `hasOnlyReadOnly` generated from model 'hasOnlyReadOnly'
--

CREATE TABLE IF NOT EXISTS `hasOnlyReadOnly` (
  `bar` TEXT DEFAULT NULL,
  `foo` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `HealthCheckResult` generated from model 'HealthCheckResult'
-- Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
--

CREATE TABLE IF NOT EXISTS `HealthCheckResult` (
  `NullableMessage` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.';

--
-- Table structure for table `inline_response_default` generated from model 'inlineUnderscoreresponseUnderscoredefault'
--

CREATE TABLE IF NOT EXISTS `inline_response_default` (
  `string` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `List` generated from model 'List'
--

CREATE TABLE IF NOT EXISTS `List` (
  `123-list` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `MapTest` generated from model 'MapTest'
--

CREATE TABLE IF NOT EXISTS `MapTest` (
  `map_map_of_string` JSON DEFAULT NULL,
  `map_of_enum_string` JSON DEFAULT NULL,
  `direct_map` JSON DEFAULT NULL,
  `indirect_map` JSON DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `MixedPropertiesAndAdditionalPropertiesClass` generated from model 'MixedPropertiesAndAdditionalPropertiesClass'
--

CREATE TABLE IF NOT EXISTS `MixedPropertiesAndAdditionalPropertiesClass` (
  `uuid` TEXT DEFAULT NULL,
  `dateTime` DATETIME DEFAULT NULL,
  `map` JSON DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Name` generated from model 'Name'
-- Model for testing model name same as property name
--

CREATE TABLE IF NOT EXISTS `Name` (
  `name` INT NOT NULL,
  `snake_case` INT DEFAULT NULL,
  `property` TEXT DEFAULT NULL,
  `123Number` INT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='Model for testing model name same as property name';

--
-- Table structure for table `NullableClass` generated from model 'NullableClass'
--

CREATE TABLE IF NOT EXISTS `NullableClass` (
  `integer_prop` INT DEFAULT NULL,
  `number_prop` DECIMAL(20, 9) DEFAULT NULL,
  `boolean_prop` TINYINT(1) DEFAULT NULL,
  `string_prop` TEXT DEFAULT NULL,
  `date_prop` DATE DEFAULT NULL,
  `datetime_prop` DATETIME DEFAULT NULL,
  `array_nullable_prop` JSON DEFAULT NULL,
  `array_and_items_nullable_prop` JSON DEFAULT NULL,
  `array_items_nullable` JSON DEFAULT NULL,
  `object_nullable_prop` JSON DEFAULT NULL,
  `object_and_items_nullable_prop` JSON DEFAULT NULL,
  `object_items_nullable` JSON DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `NumberOnly` generated from model 'NumberOnly'
--

CREATE TABLE IF NOT EXISTS `NumberOnly` (
  `JustNumber` DECIMAL(20, 9) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `ObjectWithDeprecatedFields` generated from model 'ObjectWithDeprecatedFields'
--

CREATE TABLE IF NOT EXISTS `ObjectWithDeprecatedFields` (
  `uuid` TEXT DEFAULT NULL,
  `id` DECIMAL(20, 9) DEFAULT NULL,
  `deprecatedRef` TEXT DEFAULT NULL,
  `bars` JSON DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Order` generated from model 'Order'
--

CREATE TABLE IF NOT EXISTS `Order` (
  `id` BIGINT DEFAULT NULL,
  `petId` BIGINT DEFAULT NULL,
  `quantity` INT DEFAULT NULL,
  `shipDate` DATETIME DEFAULT NULL,
  `status` ENUM('placed', 'approved', 'delivered') DEFAULT NULL COMMENT 'Order Status',
  `complete` TINYINT(1) DEFAULT false
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `OuterComposite` generated from model 'OuterComposite'
--

CREATE TABLE IF NOT EXISTS `OuterComposite` (
  `my_number` DECIMAL(20, 9) DEFAULT NULL,
  `my_string` TEXT DEFAULT NULL,
  `my_boolean` TINYINT(1) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `OuterObjectWithEnumProperty` generated from model 'OuterObjectWithEnumProperty'
--

CREATE TABLE IF NOT EXISTS `OuterObjectWithEnumProperty` (
  `value` TEXT NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Pet` generated from model 'Pet'
--

CREATE TABLE IF NOT EXISTS `Pet` (
  `id` BIGINT DEFAULT NULL,
  `category` TEXT DEFAULT NULL,
  `name` TEXT NOT NULL,
  `photoUrls` JSON NOT NULL,
  `tags` JSON DEFAULT NULL,
  `status` ENUM('available', 'pending', 'sold') DEFAULT NULL COMMENT 'pet status in the store'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `ReadOnlyFirst` generated from model 'ReadOnlyFirst'
--

CREATE TABLE IF NOT EXISTS `ReadOnlyFirst` (
  `bar` TEXT DEFAULT NULL,
  `baz` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Return` generated from model 'Return'
-- Model for testing reserved words
--

CREATE TABLE IF NOT EXISTS `Return` (
  `return` INT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='Model for testing reserved words';

--
-- Table structure for table `_special_model.name_` generated from model 'UnderscorespecialUnderscoremodelPeriodnameUnderscore'
--

CREATE TABLE IF NOT EXISTS `_special_model.name_` (
  `$special[property.name]` BIGINT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `Tag` generated from model 'Tag'
--

CREATE TABLE IF NOT EXISTS `Tag` (
  `id` BIGINT DEFAULT NULL,
  `name` TEXT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `User` generated from model 'User'
--

CREATE TABLE IF NOT EXISTS `User` (
  `id` BIGINT DEFAULT NULL,
  `username` TEXT DEFAULT NULL,
  `firstName` TEXT DEFAULT NULL,
  `lastName` TEXT DEFAULT NULL,
  `email` TEXT DEFAULT NULL,
  `password` TEXT DEFAULT NULL,
  `phone` TEXT DEFAULT NULL,
  `userStatus` INT DEFAULT NULL COMMENT 'User Status'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;


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
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

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
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

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
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `oauth_refresh_tokens`
--
CREATE TABLE IF NOT EXISTS `oauth_refresh_tokens` (
  `refresh_token`       VARCHAR(40)    NOT NULL,
  `client_id`           VARCHAR(80)    DEFAULT NULL,
  `user_id`             VARCHAR(80)    DEFAULT NULL,
  `expires`             TIMESTAMP      on update CURRENT_TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `scope`               VARCHAR(4000)  DEFAULT NULL,
  PRIMARY KEY (`refresh_token`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

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
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `oauth_scopes`
--
CREATE TABLE IF NOT EXISTS `oauth_scopes` (
  `scope`               VARCHAR(80)  NOT NULL,
  `is_default`          TINYINT(1)   DEFAULT NULL,
  PRIMARY KEY (`scope`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `oauth_jwt`
--
CREATE TABLE IF NOT EXISTS `oauth_jwt` (
  `client_id`           VARCHAR(80)    NOT NULL,
  `subject`             VARCHAR(80)    DEFAULT NULL,
  `public_key`          VARCHAR(2000)  NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `oauth_jti`
--
CREATE TABLE IF NOT EXISTS `oauth_jti` (
  `issuer`              VARCHAR(80)    NOT NULL,
  `subject`             VARCHAR(80)    DEFAULT NULL,
  `audience`            VARCHAR(80)    DEFAULT NULL,
  `expires`             TIMESTAMP      NOT NULL,
  `jti`                 VARCHAR(2000)  NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `oauth_public_keys`
--
CREATE TABLE IF NOT EXISTS `oauth_public_keys` (
  `client_id`            VARCHAR(80)    DEFAULT NULL,
  `public_key`           VARCHAR(2000)  DEFAULT NULL,
  `private_key`          VARCHAR(2000)  DEFAULT NULL,
  `encryption_algorithm` VARCHAR(100)   DEFAULT 'RS256'
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
