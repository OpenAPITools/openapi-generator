<?xml version="1.0" encoding="UTF-8"?>
<phpunit backupGlobals="false"
         backupStaticAttributes="false"
         bootstrap="vendor/autoload.php">
  <testsuites>
    <testsuite name="PHP_CodeCoverage">
      <directory suffix="Test.php">tests/PHP</directory>
    </testsuite>
  </testsuites>

  <logging>
    <log type="coverage-html" target="build/coverage"/>
    <log type="coverage-clover" target="build/logs/clover.xml"/>
    <log type="junit" target="build/logs/junit.xml" logIncompleteSkipped="false"/>
  </logging>

  <filter>
    <whitelist addUncoveredFilesFromWhitelist="true">
      <directory suffix=".php">src</directory>
    </whitelist>
  </filter>
</phpunit>

