<?php
/**
 * Generic_Sniffs_PHP_UpperCaseConstantSniff.
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
 * Generic_Sniffs_PHP_UpperCaseConstantSniff.
 *
 * Checks that all uses of TRUE, FALSE and NULL are uppercase.
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
class Generic_Sniffs_PHP_UpperCaseConstantSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(
                T_TRUE,
                T_FALSE,
                T_NULL,
               );

    }//end register()


    /**
     * Processes this sniff, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token in the
     *                                        stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens   = $phpcsFile->getTokens();
        $keyword  = $tokens[$stackPtr]['content'];
        $expected = strtoupper($keyword);
        if ($keyword !== $expected) {
            if ($keyword === strtolower($keyword)) {
                $phpcsFile->recordMetric($stackPtr, 'PHP constant case', 'lower');
            } else {
                $phpcsFile->recordMetric($stackPtr, 'PHP constant case', 'mixed');
            }

            $error = 'TRUE, FALSE and NULL must be uppercase; expected "%s" but found "%s"';
            $data  = array(
                      $expected,
                      $keyword,
                     );

            $fix = $phpcsFile->addFixableError($error, $stackPtr, 'Found', $data);
            if ($fix === true) {
                $phpcsFile->fixer->replaceToken($stackPtr, $expected);
            }
        } else {
            $phpcsFile->recordMetric($stackPtr, 'PHP constant case', 'upper');
        }

    }//end process()


}//end class
