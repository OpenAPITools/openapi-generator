--TEST--
GH-1437: Risky test messages mask failures
--FILE--
<?php

$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = 'Issue1437Test';
$_SERVER['argv'][3] = dirname(__FILE__).'/1437/Issue1437Test.php';

require __DIR__ . '/../../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

F

Time: %s, Memory: %sMb

There was 1 failure:

1) Issue1437Test::testFailure
Failed asserting that false is true.

%sIssue1437Test.php:%i

FAILURES!
Tests: 1, Assertions: 1, Failures: 1.
