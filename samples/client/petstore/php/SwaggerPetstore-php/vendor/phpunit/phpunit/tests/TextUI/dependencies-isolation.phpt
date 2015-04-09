--TEST--
phpunit --process-isolation --verbose DependencyTestSuite ../_files/DependencyTestSuite.php
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '--process-isolation';
$_SERVER['argv'][3] = '--verbose';
$_SERVER['argv'][4] = 'DependencyTestSuite';
$_SERVER['argv'][5] = dirname(dirname(__FILE__)) . '/_files/DependencyTestSuite.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

...FSS

Time: %s, Memory: %sMb

There was 1 failure:

1) DependencyFailureTest::testOne

%s:%i

--

There were 2 skipped tests:

1) DependencyFailureTest::testTwo
This test depends on "DependencyFailureTest::testOne" to pass.

2) DependencyFailureTest::testThree
This test depends on "DependencyFailureTest::testTwo" to pass.

FAILURES!
Tests: 4, Assertions: 0, Failures: 1, Skipped: 2.
