<?php
/**
 * PSR2_Sniffs_Files_ClosingTagsSniff.
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
 * PSR2_Sniffs_Files_LineEndingsSniff.
 *
 * Checks that the file does not end with a closing tag.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PSR2_Sniffs_Files_ClosingTagSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_OPEN_TAG);

    }//end register()


    /**
     * Processes this sniff, when one of its tokens is encountered.
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

        // Make sure this file only contains PHP code.
        for ($i = 0; $i < $phpcsFile->numTokens; $i++) {
            if ($tokens[$i]['code'] === T_INLINE_HTML
                && trim($tokens[$i]['content']) !== ''
            ) {
                return $phpcsFile->numTokens;
            }
        }

        // Find the last non-empty token.
        for ($last = ($phpcsFile->numTokens - 1); $last > 0; $last--) {
            if (trim($tokens[$last]['content']) !== '') {
                break;
            }
        }

        if ($tokens[$last]['code'] === T_CLOSE_TAG) {
            $error = 'A closing tag is not permitted at the end of a PHP file';
            $fix   = $phpcsFile->addFixableError($error, $last, 'NotAllowed');
            if ($fix === true) {
                $phpcsFile->fixer->replaceToken($last, '');
            }

            $phpcsFile->recordMetric($stackPtr, 'PHP closing tag at end of PHP-only file', 'yes');
        } else {
            $phpcsFile->recordMetric($stackPtr, 'PHP closing tag at end of PHP-only file', 'no');
        }

        // Ignore the rest of the file.
        return $phpcsFile->numTokens;

    }//end process()


}//end class
