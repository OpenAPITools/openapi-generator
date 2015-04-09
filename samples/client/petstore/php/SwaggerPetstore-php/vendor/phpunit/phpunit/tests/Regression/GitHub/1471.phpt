--TEST--
GH-1471: Output made while test is running is printed although expectOutputString() is used when an assertion fails
--FILE--
<?php

$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = 'Issue1471Test';
$_SERVER['argv'][3] = __DIR__ . '/1471/Issue1471Test.php';

require __DIR__ . '/../../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

F

Time: %s, Memory: %sMb

There was 1 failure:

1) Issue1471Test::testFailure
Failed asserting that false is true.

%s/Issue1471Test.php:10

FAILURES!
Tests: 1, Assertions: 1, Failures: 1.
