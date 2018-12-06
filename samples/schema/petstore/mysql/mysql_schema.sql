/* SET SQL_MODE = "NO_AUTO_VALUE_ON_ZERO"; */
/* SET AUTOCOMMIT = 0; */
/* START TRANSACTION; */
/* SET time_zone = "+00:00"; */

-- --------------------------------------------------------

--
-- Table structure for table `$special[model.name]` generated from model 'DollarspecialLeft_Square_BracketmodelPeriodnameRight_Square_Bracket'
--

CREATE TABLE IF NOT EXISTS `$special[model.name]` (
  `$special[property.name]` BIGINT DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

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
-- Table structure for table `Dog` generated from model 'Dog'
--

CREATE TABLE IF NOT EXISTS `Dog` (
  `className` TEXT NOT NULL,
  `color` TEXT,
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
  `outerEnum` TEXT DEFAULT NULL
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
-- Table structure for table `format_test` generated from model 'formatUnderscoretest'
--

CREATE TABLE IF NOT EXISTS `format_test` (
  `integer` TINYINT UNSIGNED DEFAULT NULL,
  `int32` TINYINT UNSIGNED DEFAULT NULL,
  `int64` BIGINT DEFAULT NULL,
  `number` DECIMAL(20, 9) UNSIGNED NOT NULL,
  `float` DECIMAL(20, 9) UNSIGNED DEFAULT NULL,
  `double` DECIMAL(20, 9) UNSIGNED DEFAULT NULL,
  `string` TEXT DEFAULT NULL,
  `byte` MEDIUMBLOB NOT NULL,
  `binary` MEDIUMBLOB DEFAULT NULL,
  `date` DATE NOT NULL,
  `dateTime` DATETIME DEFAULT NULL,
  `uuid` TEXT DEFAULT NULL,
  `password` VARCHAR(64) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

--
-- Table structure for table `hasOnlyReadOnly` generated from model 'hasOnlyReadOnly'
--

CREATE TABLE IF NOT EXISTS `hasOnlyReadOnly` (
  `bar` TEXT DEFAULT NULL,
  `foo` TEXT DEFAULT NULL
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
-- Table structure for table `NumberOnly` generated from model 'NumberOnly'
--

CREATE TABLE IF NOT EXISTS `NumberOnly` (
  `JustNumber` DECIMAL(20, 9) DEFAULT NULL
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

