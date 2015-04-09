<?php
class PHPCS_Sniffs_Whitespace_ConcatenationSpacingSniff implements PHP_CodeSniffer_Sniff
{
    public function register()
    {
        return array(T_STRING_CONCAT);
    }

    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        if ($tokens[($stackPtr - 1)]['code'] !== T_WHITESPACE ||
            $tokens[($stackPtr + 1)]['code'] !== T_WHITESPACE) {

            $phpcsFile->addError(
              'Concatenation operator must be surrounded by whitespace',
              $stackPtr
            );
        }
    }
}
