--TEST--
GH-1348: STDOUT/STDERR IO streams should exist in process isolation
--SKIPIF--
<?php
if (defined('HHVM_VERSION'))
    print "skip: PHP runtime required";
?>
--FILE--
<?php

$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][] = '--report-useless-tests';
$_SERVER['argv'][] = '--process-isolation';
$_SERVER['argv'][] = 'Issue1348Test';
$_SERVER['argv'][] = __DIR__ . '/1348/Issue1348Test.php';

require __DIR__ . '/../../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

.
STDOUT does not break test result
E

Time: %s, Memory: %sMb

There was 1 error:

1) Issue1348Test::testSTDERR
PHPUnit_Framework_Exception: STDERR works as usual.

FAILURES!
Tests: 2, Assertions: 1, Errors: 1.
