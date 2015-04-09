--TEST--
GH-1330: Allow non-ambiguous shortened longopts
--FILE--
<?php

$_SERVER['argv'][1] = '--deb';
$_SERVER['argv'][2] = '--config';
$_SERVER['argv'][3] = dirname(__FILE__).'/1330/phpunit1330.xml';
$_SERVER['argv'][4] = 'Issue1330Test';
$_SERVER['argv'][5] = dirname(__FILE__).'/1330/Issue1330Test.php';

require __DIR__ . '/../../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

Configuration read from %s


Starting test 'Issue1330Test::testTrue'.
.

Time: %s, Memory: %sMb

OK (1 test, 1 assertion)
