--TEST--
GH-1570: Test that prints output is marked as failure and not as risky when --disallow-test-output is used
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '--disallow-test-output';
$_SERVER['argv'][3] = 'Issue1570Test';
$_SERVER['argv'][4] = dirname(__FILE__) . '/1570/Issue1570Test.php';

require __DIR__ . '/../../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

R*

Time: %s, Memory: %sMb

OK, but incomplete, skipped, or risky tests!
Tests: 1, Assertions: 0, Risky: 1.
