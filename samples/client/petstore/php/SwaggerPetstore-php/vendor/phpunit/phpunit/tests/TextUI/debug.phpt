--TEST--
phpunit --debug BankAccountTest ../_files/BankAccountTest.php
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '--debug';
$_SERVER['argv'][3] = 'BankAccountTest';
$_SERVER['argv'][4] = dirname(__FILE__).'/../_files/BankAccountTest.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.


Starting test 'BankAccountTest::testBalanceIsInitiallyZero'.
.
Starting test 'BankAccountTest::testBalanceCannotBecomeNegative'.
.
Starting test 'BankAccountTest::testBalanceCannotBecomeNegative2'.
.

Time: %s, Memory: %sMb

OK (3 tests, 3 assertions)
