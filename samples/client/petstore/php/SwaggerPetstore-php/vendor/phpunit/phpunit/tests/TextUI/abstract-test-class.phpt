--TEST--
phpunit AbstractTest ../_files/AbstractTest.php
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = 'AbstractTest';
$_SERVER['argv'][3] = dirname(dirname(__FILE__)) . '/_files/AbstractTest.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

F

Time: %s, Memory: %sMb

There was 1 failure:

1) Warning
Cannot instantiate class "AbstractTest".

FAILURES!
Tests: 1, Assertions: 0, Failures: 1.
