<?php
$coverage->stop();

$writer = new PHP_CodeCoverage_Report_HTML;
$writer->process($coverage, '/tmp/coverage');
