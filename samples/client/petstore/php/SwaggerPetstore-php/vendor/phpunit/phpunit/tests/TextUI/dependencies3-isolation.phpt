--TEST--
phpunit --process-isolation MultiDependencyTest ../_files/MultiDependencyTest.php
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '--process-isolation';
$_SERVER['argv'][3] = 'MultiDependencyTest';
$_SERVER['argv'][4] = dirname(dirname(__FILE__)) . '/_files/MultiDependencyTest.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

...

Time: %s, Memory: %sMb

OK (3 tests, 2 assertions)
