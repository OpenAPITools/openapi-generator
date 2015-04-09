<?php
/**
 * Generic_Sniffs_Formatting_DisallowMultipleStatementsSniff.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Generic_Sniffs_Formatting_DisallowMultipleStatementsSniff.
 *
 * Ensures each statement is on a line by itself.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Generic_Sniffs_Formatting_DisallowMultipleStatementsSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_SEMICOLON);

    }//end register()


    /**
     * Processes this test, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token in
     *                                        the stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        $prev = $phpcsFile->findPrevious(array(T_SEMICOLON, T_OPEN_TAG), ($stackPtr - 1));
        if ($prev === false || $tokens[$prev]['code'] === T_OPEN_TAG) {
            $phpcsFile->recordMetric($stackPtr, 'Multiple statements on same line', 'no');
            return;
        }

        // Ignore multiple statements in a FOR condition.
        if (isset($tokens[$stackPtr]['nested_parenthesis']) === true) {
            foreach ($tokens[$stackPtr]['nested_parenthesis'] as $bracket) {
                if (isset($tokens[$bracket]['parenthesis_owner']) === false) {
                    // Probably a closure sitting inside a function call.
                    continue;
                }

                $owner = $tokens[$bracket]['parenthesis_owner'];
                if ($tokens[$owner]['code'] === T_FOR) {
                    return;
                }
            }
        }

        if ($tokens[$prev]['line'] === $tokens[$stackPtr]['line']) {
            $phpcsFile->recordMetric($stackPtr, 'Multiple statements on same line', 'yes');

            $error = 'Each PHP statement must be on a line by itself';
            $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'SameLine');
            if ($fix === true) {
                $phpcsFile->fixer->beginChangeset();
                $phpcsFile->fixer->addNewline($prev);
                if ($tokens[($prev + 1)]['code'] === T_WHITESPACE) {
                    $phpcsFile->fixer->replaceToken(($prev + 1), '');
                }

                $phpcsFile->fixer->endChangeset();
            }
        } else {
            $phpcsFile->recordMetric($stackPtr, 'Multiple statements on same line', 'no');
        }

    }//end process()


}//end class
