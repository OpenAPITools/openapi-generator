--TEST--
phpunit StackTest ../_files/StackTest.php
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = 'StackTest';
$_SERVER['argv'][3] = dirname(dirname(__FILE__)) . '/_files/StackTest.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

..

Time: %s, Memory: %sMb

OK (2 tests, 5 assertions)
