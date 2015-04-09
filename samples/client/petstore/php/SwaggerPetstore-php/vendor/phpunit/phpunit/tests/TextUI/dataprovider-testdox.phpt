--TEST--
phpunit --testdox DataProviderTest ../_files/DataProviderTest.php
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '--testdox';
$_SERVER['argv'][3] = 'DataProviderTest';
$_SERVER['argv'][4] = dirname(dirname(__FILE__)) . '/_files/DataProviderTest.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

DataProvider
 [ ] Add
