--TEST--
#523: assertAttributeEquals does not work with classes extending ArrayIterator
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = 'Issue523Test';
$_SERVER['argv'][3] = dirname(__FILE__).'/523/Issue523Test.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.

.

Time: %s, Memory: %sMb

OK (1 test, 1 assertion)
