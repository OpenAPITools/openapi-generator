<?php
class PHPCS_Sniffs_ControlStructures_ControlSignatureSniff extends PHP_CodeSniffer_Standards_AbstractPatternSniff
{
    public function __construct()
    {
        parent::__construct(true);
    }

    protected function getPatterns()
    {
        return array(
          'do {EOL...} while (...);EOL',
          'while (...) {EOL',
          'for (...) {EOL',
          'if (...) {EOL',
          'foreach (...) {EOL',
          '}EOLelse if (...) {EOL',
          '}EOLelse {EOL',
          'do {EOL',
        );
    }
}
