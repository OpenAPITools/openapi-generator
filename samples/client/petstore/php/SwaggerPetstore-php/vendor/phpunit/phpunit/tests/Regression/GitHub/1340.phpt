--TEST--
GH-1340: Process isolation blocks infinitely upon fatal error
--FILE--
<?php

$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '-d';
$_SERVER['argv'][3] = 'error_log=';
$_SERVER['argv'][4] = 'Issue1340Test';
$_SERVER['argv'][5] = dirname(__FILE__).'/1340/Issue1340Test.php';

require __DIR__ . '/../../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.
%A
.E.EE

Time: %s, Memory: %sMb

There were 3 errors:

1) Issue1340Test::testLargeStderrOutputDoesNotBlockInIsolation
PHPUnit_Framework_Exception: testLargeStderrOutputDoesNotBlockInIsolation: stderr:%d
%A
2) Issue1340Test::testPhpNoticeWithStderrOutputIsAnError
PHPUnit_Framework_Exception: shutdown: stderr:%d
%A
3) Issue1340Test::testFatalErrorDoesNotPass
PHPUnit_Framework_Exception: Fatal error: Call to undefined function undefined_function() in %s on line %d
%A
shutdown: stderr:%d
%A
FAILURES!
Tests: 5, Assertions: 3, Errors: 3.