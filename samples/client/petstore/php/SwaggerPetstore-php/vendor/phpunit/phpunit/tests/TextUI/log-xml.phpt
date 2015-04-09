--TEST--
phpunit --log-junit php://stdout BankAccountTest ../_files/BankAccountTest.php
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '--log-junit';
$_SERVER['argv'][3] = 'php://stdout';
$_SERVER['argv'][4] = 'BankAccountTest';
$_SERVER['argv'][5] = dirname(__FILE__).'/../_files/BankAccountTest.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

...<?xml version="1.0" encoding="UTF-8"?>
<testsuites>
  <testsuite name="BankAccountTest" file="%sBankAccountTest.php" tests="3" assertions="3" failures="0" errors="0" time="%f">
    <testcase name="testBalanceIsInitiallyZero" class="BankAccountTest" file="%sBankAccountTest.php" line="35" assertions="1" time="%f"/>
    <testcase name="testBalanceCannotBecomeNegative" class="BankAccountTest" file="%sBankAccountTest.php" line="45" assertions="1" time="%f"/>
    <testcase name="testBalanceCannotBecomeNegative2" class="BankAccountTest" file="%sBankAccountTest.php" line="63" assertions="1" time="%f"/>
  </testsuite>
</testsuites>


Time: %s, Memory: %sMb

OK (3 tests, 3 assertions)
