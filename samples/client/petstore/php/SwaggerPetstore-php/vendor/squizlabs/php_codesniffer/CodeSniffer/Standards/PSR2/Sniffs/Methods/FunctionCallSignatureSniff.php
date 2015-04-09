<?php
/**
 * PSR2_Sniffs_Methods_FunctionCallSignatureSniff.
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
 * PSR2_Sniffs_Methods_FunctionCallSignatureSniff.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PSR2_Sniffs_Methods_FunctionCallSignatureSniff extends PEAR_Sniffs_Functions_FunctionCallSignatureSniff
{

    /**
     * If TRUE, multiple arguments can be defined per line in a multi-line call.
     *
     * @var bool
     */
    public $allowMultipleArguments = false;


    /**
     * Processes single-line calls.
     *
     * @param PHP_CodeSniffer_File $phpcsFile   The file being scanned.
     * @param int                  $stackPtr    The position of the current token
     *                                          in the stack passed in $tokens.
     * @param int                  $openBracket The position of the opening bracket
     *                                          in the stack passed in $tokens.
     * @param array                $tokens      The stack of tokens that make up
     *                                          the file.
     *
     * @return void
     */
    public function isMultiLineCall(PHP_CodeSniffer_File $phpcsFile, $stackPtr, $openBracket, $tokens)
    {
        // If the first argument is on a new line, this is a multi-line
        // function call, even if there is only one argument.
        $next = $phpcsFile->findNext(PHP_CodeSniffer_Tokens::$emptyTokens, ($openBracket + 1), null, true);
        if ($tokens[$next]['line'] !== $tokens[$stackPtr]['line']) {
            return true;
        }

        $closeBracket = $tokens[$openBracket]['parenthesis_closer'];

        $end = $phpcsFile->findEndOfStatement($openBracket + 1);
        while ($tokens[$end]['code'] === T_COMMA) {
            // If the next bit of code is not on the same line, this is a
            // multi-line function call.
            $next = $phpcsFile->findNext(PHP_CodeSniffer_Tokens::$emptyTokens, ($end + 1), $closeBracket, true);
            if ($next === false) {
                return false;
            }

            if ($tokens[$next]['line'] !== $tokens[$end]['line']) {
                return true;
            }

            $end = $phpcsFile->findEndOfStatement($next);
        }

        // We've reached the last argument, so see if the next content
        // (should be the close bracket) is also on the same line.
        $next = $phpcsFile->findNext(PHP_CodeSniffer_Tokens::$emptyTokens, ($end + 1), $closeBracket, true);
        if ($next !== false && $tokens[$next]['line'] !== $tokens[$end]['line']) {
            return true;
        }

        return false;

    }//end isMultiLineCall()


}//end class
