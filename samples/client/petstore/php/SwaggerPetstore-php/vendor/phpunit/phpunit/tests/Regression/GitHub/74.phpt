--TEST--
GH-74: catchable fatal error in 3.5
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '--process-isolation';
$_SERVER['argv'][3] = 'Issue74Test';
$_SERVER['argv'][4] = dirname(__FILE__).'/74/Issue74Test.php';

require __DIR__ . '/../../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

E

Time: %s, Memory: %sMb

There was 1 error:

1) Issue74Test::testCreateAndThrowNewExceptionInProcessIsolation
NewException: Testing GH-74

%sIssue74Test.php:7

FAILURES!
Tests: 1, Assertions: 0, Errors: 1.
