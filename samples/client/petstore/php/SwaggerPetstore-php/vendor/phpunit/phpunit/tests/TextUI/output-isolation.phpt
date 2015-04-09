--TEST--
phpunit --process-isolation --filter testExpectOutputStringFooActualFoo ../_files/OutputTestCase.php
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '--process-isolation';
$_SERVER['argv'][3] = '--filter';
$_SERVER['argv'][4] = 'testExpectOutputStringFooActualFoo';
$_SERVER['argv'][5] = dirname(__FILE__).'/../_files/OutputTestCase.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

.

Time: %s, Memory: %sMb

OK (1 test, 1 assertion)
