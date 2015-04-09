--TEST--
phpunit --report-useless-tests --process-isolation IncompleteTest ../_files/IncompleteTest.php
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '--report-useless-tests';
$_SERVER['argv'][3] = '--process-isolation';
$_SERVER['argv'][4] = 'NothingTest';
$_SERVER['argv'][5] = dirname(dirname(__FILE__)) . '/_files/NothingTest.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

R

Time: %s, Memory: %sMb

OK, but incomplete, skipped, or risky tests!
Tests: 1, Assertions: 0, Risky: 1.
