--TEST--
GH-1337: Data Provider with \ at the end of the name breaks with process isolation
--FILE--
<?php

$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = '--process-isolation';
$_SERVER['argv'][3] = 'Issue1337Test';
$_SERVER['argv'][4] = dirname(__FILE__).'/1337/Issue1337Test.php';

require __DIR__ . '/../../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

..

Time: %s, Memory: %sMb

OK (2 tests, 2 assertions)