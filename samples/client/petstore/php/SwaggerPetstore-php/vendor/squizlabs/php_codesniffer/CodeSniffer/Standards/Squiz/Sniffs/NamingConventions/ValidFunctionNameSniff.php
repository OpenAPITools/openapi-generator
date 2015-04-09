<?php
/**
 * Squiz_Sniffs_NamingConventions_ValidFunctionNameSniff.
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

if (class_exists('PEAR_Sniffs_NamingConventions_ValidFunctionNameSniff', true) === false) {
    throw new PHP_CodeSniffer_Exception('Class PEAR_Sniffs_NamingConventions_ValidFunctionNameSniff not found');
}

/**
 * Squiz_Sniffs_NamingConventions_ValidFunctionNameSniff.
 *
 * Ensures method names are correct depending on whether they are public
 * or private, and that functions are named correctly.
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
class Squiz_Sniffs_NamingConventions_ValidFunctionNameSniff extends PEAR_Sniffs_NamingConventions_ValidFunctionNameSniff
{


    /**
     * Processes the tokens outside the scope.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being processed.
     * @param int                  $stackPtr  The position where this token was
     *                                        found.
     *
     * @return void
     */
    protected function processTokenOutsideScope(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $functionName = $phpcsFile->getDeclarationName($stackPtr);
        if ($functionName === null) {
            return;
        }

        $errorData = array($functionName);

        // Does this function claim to be magical?
        if (preg_match('|^__|', $functionName) !== 0) {
            $error = 'Function name "%s" is invalid; only PHP magic methods should be prefixed with a double underscore';
            $phpcsFile->addError($error, $stackPtr, 'DoubleUnderscore', $errorData);
            return;
        }

        if (PHP_CodeSniffer::isCamelCaps($functionName, false, true, false) === false) {
            $error = 'Function name "%s" is not in camel caps format';
            $phpcsFile->addError($error, $stackPtr, 'NotCamelCaps', $errorData);
        }

    }//end processTokenOutsideScope()


}//end class
