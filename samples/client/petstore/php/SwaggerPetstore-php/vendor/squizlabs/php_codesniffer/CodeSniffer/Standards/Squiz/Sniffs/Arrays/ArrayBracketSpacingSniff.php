<?php
/**
 * Squiz_Sniffs_Arrays_ArrayBracketSpacingSniff.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Marc McIntyre <mmcintyre@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Squiz_Sniffs_Arrays_ArrayBracketSpacingSniff.
 *
 * Ensure that there are no spaces around square brackets.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Marc McIntyre <mmcintyre@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Squiz_Sniffs_Arrays_ArrayBracketSpacingSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(
                T_OPEN_SQUARE_BRACKET,
                T_CLOSE_SQUARE_BRACKET,
               );

    }//end register()


    /**
     * Processes this sniff, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The current file being checked.
     * @param int                  $stackPtr  The position of the current token in the
     *                                        stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        // PHP 5.4 introduced a shorthand array declaration syntax, so we need
        // to ignore the these type of array declarations because this sniff is
        // only dealing with array usage.
        if ($tokens[$stackPtr]['code'] === T_OPEN_SQUARE_BRACKET) {
            $openBracket = $stackPtr;
        } else {
            $openBracket = $tokens[$stackPtr]['bracket_opener'];
        }

        $prev = $phpcsFile->findPrevious(PHP_CodeSniffer_Tokens::$emptyTokens, ($openBracket - 1), null, true);
        if ($tokens[$prev]['code'] === T_EQUAL) {
            return;
        }

        // Square brackets can not have a space before them.
        $prevType = $tokens[($stackPtr - 1)]['code'];
        if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$prevType]) === true) {
            $nonSpace = $phpcsFile->findPrevious(PHP_CodeSniffer_Tokens::$emptyTokens, ($stackPtr - 2), null, true);
            $expected = $tokens[$nonSpace]['content'].$tokens[$stackPtr]['content'];
            $found    = $phpcsFile->getTokensAsString($nonSpace, ($stackPtr - $nonSpace)).$tokens[$stackPtr]['content'];
            $error    = 'Space found before square bracket; expected "%s" but found "%s"';
            $data     = array(
                         $expected,
                         $found,
                        );
            $fix      = $phpcsFile->addFixableError($error, $stackPtr, 'SpaceBeforeBracket', $data);
            if ($fix === true) {
                $phpcsFile->fixer->replaceToken(($stackPtr - 1), '');
            }
        }

        // Open square brackets can't ever have spaces after them.
        if ($tokens[$stackPtr]['code'] === T_OPEN_SQUARE_BRACKET) {
            $nextType = $tokens[($stackPtr + 1)]['code'];
            if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$nextType]) === true) {
                $nonSpace = $phpcsFile->findNext(PHP_CodeSniffer_Tokens::$emptyTokens, ($stackPtr + 2), null, true);
                $expected = $tokens[$stackPtr]['content'].$tokens[$nonSpace]['content'];
                $found    = $phpcsFile->getTokensAsString($stackPtr, ($nonSpace - $stackPtr + 1));
                $error    = 'Space found after square bracket; expected "%s" but found "%s"';
                $data     = array(
                             $expected,
                             $found,
                            );
                $fix      = $phpcsFile->addFixableError($error, $stackPtr, 'SpaceAfterBracket', $data);
                if ($fix === true) {
                    $phpcsFile->fixer->replaceToken(($stackPtr + 1), '');
                }
            }
        }

    }//end process()


}//end class
