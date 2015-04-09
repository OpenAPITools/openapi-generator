--TEST--
GH-1216: PHPUnit bootstrap must take globals vars even when the file is specified in command line
--FILE--
<?php

$_SERVER['argv'][1] = '--configuration';
$_SERVER['argv'][2] = dirname(__FILE__).'/1216/phpunit1216.xml';
$_SERVER['argv'][3] = '--debug';
$_SERVER['argv'][4] = '--bootstrap';
$_SERVER['argv'][5] = dirname(__FILE__).'/1216/bootstrap1216.php';
$_SERVER['argv'][6] = dirname(__FILE__) . '/1216/Issue1216Test.php';

require __DIR__ . '/../../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

Configuration read from %s


Starting test 'Issue1216Test::testConfigAvailableInBootstrap'.
.

Time: %s, Memory: %sMb

OK (1 test, 1 assertion)