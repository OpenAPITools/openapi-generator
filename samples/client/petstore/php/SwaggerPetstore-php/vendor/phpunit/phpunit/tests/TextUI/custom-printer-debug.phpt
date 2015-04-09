--TEST--
phpunit -c ../_files/configuration.custom-printer.xml --debug BankAccountTest ../_files/BankAccountTest.php
--FILE--
<?php
$_SERVER['argv'][1] = '-c';
$_SERVER['argv'][2] = dirname(__FILE__).'/../_files/configuration.custom-printer.xml';
$_SERVER['argv'][3] = '--debug';
$_SERVER['argv'][4] = 'BankAccountTest';
$_SERVER['argv'][5] = dirname(__FILE__).'/../_files/BankAccountTest.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

Configuration read from %sconfiguration.custom-printer.xml


Starting test 'BankAccountTest::testBalanceIsInitiallyZero'.
.
Starting test 'BankAccountTest::testBalanceCannotBecomeNegative'.
.
Starting test 'BankAccountTest::testBalanceCannotBecomeNegative2'.
.

Time: %s, Memory: %sMb

OK (3 tests, 3 assertions)
