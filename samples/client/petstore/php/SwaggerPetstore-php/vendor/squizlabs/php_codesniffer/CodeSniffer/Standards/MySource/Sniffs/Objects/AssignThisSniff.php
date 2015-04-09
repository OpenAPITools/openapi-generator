<?php
/**
 * Ensures this is not assigned to any other var but self.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer_MySource
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Ensures this is not assigned to any other var but self.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer_MySource
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class MySource_Sniffs_Objects_AssignThisSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of tokenizers this sniff supports.
     *
     * @var array
     */
    public $supportedTokenizers = array('JS');


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_THIS);

    }//end register()


    /**
     * Processes this test, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token
     *                                        in the stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        // Ignore this.something and other uses of "this" that are not
        // direct assignments.
        $next = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 1), null, true);
        if ($tokens[$next]['code'] !== T_SEMICOLON) {
            if ($tokens[$next]['line'] === $tokens[$stackPtr]['line']) {
                return;
            }
        }

        // Something must be assigned to "this".
        $prev = $phpcsFile->findPrevious(T_WHITESPACE, ($stackPtr - 1), null, true);
        if ($tokens[$prev]['code'] !== T_EQUAL) {
            return;
        }

        // A variable needs to be assigned to "this".
        $prev = $phpcsFile->findPrevious(T_WHITESPACE, ($prev - 1), null, true);
        if ($tokens[$prev]['code'] !== T_STRING) {
            return;
        }

        // We can only assign "this" to a var called "self".
        if ($tokens[$prev]['content'] !== 'self' && $tokens[$prev]['content'] !== '_self') {
            $error = 'Keyword "this" can only be assigned to a variable called "self" or "_self"';
            $phpcsFile->addError($error, $prev, 'NotSelf');
        }

    }//end process()


}//end class
