<?php
require_once 'PHP/CodeCoverage/Autoload.php';

$coverage = new PHP_CodeCoverage;
$filter   = $coverage->filter();

$filter->addFileToBlacklist(__FILE__);
$filter->addFileToBlacklist(dirname(__FILE__) . '/auto_append.php');

$coverage->start($_SERVER['SCRIPT_FILENAME']);
