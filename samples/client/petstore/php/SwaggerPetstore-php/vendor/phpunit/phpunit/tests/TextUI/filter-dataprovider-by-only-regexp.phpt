--TEST--
phpunit --filter @false.* DataProviderFilterTest ../_files/DataProviderFilterTest.php
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '--filter';
$_SERVER['argv'][3] = '@false.*';
$_SERVER['argv'][4] = 'DataProviderFilterTest';
$_SERVER['argv'][5] = dirname(__FILE__).'/../_files/DataProviderFilterTest.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

..

Time: %s, Memory: %sMb

OK (2 tests, 2 assertions)
