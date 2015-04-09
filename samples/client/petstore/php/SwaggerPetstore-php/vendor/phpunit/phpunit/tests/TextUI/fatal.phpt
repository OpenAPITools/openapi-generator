--TEST--
phpunit FatalTest ../_files/FatalTest.php
--FILE--
<?php
$_SERVER['argv'][1] = '--no-configuration';
$_SERVER['argv'][2] = 'FatalTest';
$_SERVER['argv'][3] = dirname(dirname(__FILE__)) . '/_files/FatalTest.php';

require __DIR__ . '/../bootstrap.php';
PHPUnit_TextUI_Command::main();
?>
--EXPECTF--
PHPUnit %s by Sebastian Bergmann and contributors.


Fatal error: Call to undefined function non_existing_function() in %s
