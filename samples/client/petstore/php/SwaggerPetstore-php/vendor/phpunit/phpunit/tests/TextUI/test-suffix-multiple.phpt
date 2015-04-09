--TEST--
phpunit --test-suffix .test.php,.my.php ../_files/
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '--test-suffix';
$_SERVER['argv'][3] = '.test.php,.my.php';
$_SERVER['argv'][4] = dirname(__FILE__).'/../_files/';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

.....

Time: %s, Memory: %sMb

OK (5 tests, 3 assertions)
