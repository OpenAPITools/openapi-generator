<?php
/**
 * PEAR_Sniffs_Functions_ValidDefaultValueSniff.
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
 * PEAR_Sniffs_Functions_ValidDefaultValueSniff.
 *
 * A Sniff to ensure that parameters defined for a function that have a default
 * value come at the end of the function signature.
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
class PEAR_Sniffs_Functions_ValidDefaultValueSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_FUNCTION);

    }//end register()


    /**
     * Processes this test, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token in the
     *                                        stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        $argStart = $tokens[$stackPtr]['parenthesis_opener'];
        $argEnd   = $tokens[$stackPtr]['parenthesis_closer'];

        // Flag for when we have found a default in our arg list.
        // If there is a value without a default after this, it is an error.
        $defaultFound = false;

        $nextArg = $argStart;
        while (($nextArg = $phpcsFile->findNext(T_VARIABLE, ($nextArg + 1), $argEnd)) !== false) {
            if ($tokens[($nextArg - 1)]['code'] === T_ELLIPSIS) {
                continue;
            }

            $argHasDefault = self::_argHasDefault($phpcsFile, $nextArg);
            if ($argHasDefault === false && $defaultFound === true) {
                $error = 'Arguments with default values must be at the end of the argument list';
                $phpcsFile->addError($error, $nextArg, 'NotAtEnd');
                return;
            }

            if ($argHasDefault === true) {
                $defaultFound = true;
            }
        }

    }//end process()


    /**
     * Returns true if the passed argument has a default value.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $argPtr    The position of the argument
     *                                        in the stack.
     *
     * @return bool
     */
    private static function _argHasDefault(PHP_CodeSniffer_File $phpcsFile, $argPtr)
    {
        $tokens    = $phpcsFile->getTokens();
        $nextToken = $phpcsFile->findNext(PHP_CodeSniffer_Tokens::$emptyTokens, ($argPtr + 1), null, true);
        if ($tokens[$nextToken]['code'] !== T_EQUAL) {
            return false;
        }

        return true;

    }//end _argHasDefault()


}//end class
