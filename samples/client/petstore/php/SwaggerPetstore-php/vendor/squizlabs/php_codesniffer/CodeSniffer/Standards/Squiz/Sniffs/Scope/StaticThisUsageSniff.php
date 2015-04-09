<?php
/**
 * Squiz_Sniffs_Scope_StaticThisUsageSniff.
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

if (class_exists('PHP_CodeSniffer_Standards_AbstractScopeSniff', true) === false) {
    throw new PHP_CodeSniffer_Exception('Class PHP_CodeSniffer_Standards_AbstractScopeSniff not found');
}

/**
 * Squiz_Sniffs_Scope_StaticThisUsageSniff.
 *
 * Checks for usage of "$this" in static methods, which will cause
 * runtime errors.
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
class Squiz_Sniffs_Scope_StaticThisUsageSniff extends PHP_CodeSniffer_Standards_AbstractScopeSniff
{


    /**
     * Constructs the test with the tokens it wishes to listen for.
     */
    public function __construct()
    {
        parent::__construct(array(T_CLASS), array(T_FUNCTION));

    }//end __construct()


    /**
     * Processes this test, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The current file being scanned.
     * @param int                  $stackPtr  The position of the current token in the
     *                                        stack passed in $tokens.
     * @param int                  $currScope A pointer to the start of the scope.
     *
     * @return void
     */
    public function processTokenWithinScope(PHP_CodeSniffer_File $phpcsFile, $stackPtr, $currScope)
    {
        $tokens   = $phpcsFile->getTokens();
        $function = $tokens[($stackPtr + 2)];

        if ($function['code'] !== T_STRING) {
            return;
        }

        $functionName = $function['content'];
        $classOpener  = $tokens[$currScope]['scope_condition'];
        $className    = $tokens[($classOpener + 2)]['content'];

        $methodProps = $phpcsFile->getMethodProperties($stackPtr);

        if ($methodProps['is_static'] === true) {
            if (isset($tokens[$stackPtr]['scope_closer']) === false) {
                // There is no scope opener or closer, so the function
                // must be abstract.
                return;
            }

            $thisUsage = $stackPtr;
            while (($thisUsage = $phpcsFile->findNext(array(T_VARIABLE), ($thisUsage + 1), $tokens[$stackPtr]['scope_closer'], false, '$this')) !== false) {
                if ($thisUsage === false) {
                    return;
                }

                $error = 'Usage of "$this" in static methods will cause runtime errors';
                $phpcsFile->addError($error, $thisUsage, 'Found');
            }
        }//end if

    }//end processTokenWithinScope()


}//end class
