<?php
/**
 * Squiz_Sniffs_Classes_ValidClassNameSniff.
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
 * Squiz_Sniffs_Classes_ValidClassNameSniff.
 *
 * Ensures classes are in camel caps, and the first letter is capitalised
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
class Squiz_Sniffs_Classes_ValidClassNameSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(
                T_CLASS,
                T_INTERFACE,
               );

    }//end register()


    /**
     * Processes this test, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The current file being processed.
     * @param int                  $stackPtr  The position of the current token in the
     *                                        stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        if (isset($tokens[$stackPtr]['scope_opener']) === false) {
            $error = 'Possible parse error: %s missing opening or closing brace';
            $data  = array($tokens[$stackPtr]['content']);
            $phpcsFile->addWarning($error, $stackPtr, 'MissingBrace', $data);
            return;
        }

        // Determine the name of the class or interface. Note that we cannot
        // simply look for the first T_STRING because a class name
        // starting with the number will be multiple tokens.
        $opener    = $tokens[$stackPtr]['scope_opener'];
        $nameStart = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 1), $opener, true);
        $nameEnd   = $phpcsFile->findNext(T_WHITESPACE, $nameStart, $opener);
        $name      = trim($phpcsFile->getTokensAsString($nameStart, ($nameEnd - $nameStart)));

        // Check for camel caps format.
        $valid = PHP_CodeSniffer::isCamelCaps($name, true, true, false);
        if ($valid === false) {
            $type  = ucfirst($tokens[$stackPtr]['content']);
            $error = '%s name "%s" is not in camel caps format';
            $data  = array(
                      $type,
                      $name,
                     );
            $phpcsFile->addError($error, $stackPtr, 'NotCamelCaps', $data);
            $phpcsFile->recordMetric($stackPtr, 'CamelCase class name', 'no');
        } else {
            $phpcsFile->recordMetric($stackPtr, 'CamelCase class name', 'yes');
        }

    }//end process()


}//end class
