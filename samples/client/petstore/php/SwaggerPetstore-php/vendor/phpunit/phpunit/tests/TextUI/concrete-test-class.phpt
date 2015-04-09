--TEST--
phpunit ConcreteTest ../_files/ConcreteTest.php
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = 'ConcreteTest';
$_SERVER['argv'][3] = dirname(dirname(__FILE__)) . '/_files/ConcreteTest.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

..

Time: %s, Memory: %sMb

OK (2 tests, 0 assertions)
