--TEST--
phpunit FatalTest ../_files/FatalTest.php
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '--process-isolation';
$_SERVER['argv'][3] = 'FatalTest';
$_SERVER['argv'][4] = dirname(dirname(__FILE__)) . '/_files/FatalTest.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

E

Time: %s, Memory: %sMb

There was 1 error:

1) FatalTest::testFatalError
%s

FAILURES!
Tests: 1, Assertions: 0, Errors: 1.
