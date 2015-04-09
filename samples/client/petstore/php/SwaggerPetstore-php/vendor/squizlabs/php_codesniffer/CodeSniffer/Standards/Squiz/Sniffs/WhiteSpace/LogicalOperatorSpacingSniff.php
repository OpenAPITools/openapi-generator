<?php
/**
 * Sniffs_Squiz_WhiteSpace_OperatorSpacingSniff.
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
 * Sniffs_Squiz_WhiteSpace_OperatorSpacingSniff.
 *
 * Verifies that operators have valid spacing surrounding them.
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
class Squiz_Sniffs_WhiteSpace_LogicalOperatorSpacingSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of tokenizers this sniff supports.
     *
     * @var array
     */
    public $supportedTokenizers = array(
                                   'PHP',
                                   'JS',
                                  );


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return PHP_CodeSniffer_Tokens::$booleanOperators;

    }//end register()


    /**
     * Processes this sniff, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The current file being checked.
     * @param int                  $stackPtr  The position of the current token
     *                                        in the stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        // Check there is one space before the operator.
        if ($tokens[($stackPtr - 1)]['code'] !== T_WHITESPACE) {
            $error = 'Expected 1 space before logical operator; 0 found';
            $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'NoSpaceBefore');
            if ($fix === true) {
                $phpcsFile->fixer->addContentBefore($stackPtr, ' ');
            }
        } else {
            $prev = $phpcsFile->findPrevious(T_WHITESPACE, ($stackPtr - 1), null, true);
            if ($tokens[$stackPtr]['line'] === $tokens[$prev]['line']
                && strlen($tokens[($stackPtr - 1)]['content']) !== 1
            ) {
                $found = strlen($tokens[($stackPtr - 1)]['content']);
                $error = 'Expected 1 space before logical operator; %s found';
                $data  = array($found);
                $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'TooMuchSpaceBefore', $data);
                if ($fix === true) {
                    $phpcsFile->fixer->replaceToken(($stackPtr - 1), ' ');
                }
            }
        }

        // Check there is one space after the operator.
        if ($tokens[($stackPtr + 1)]['code'] !== T_WHITESPACE) {
            $error = 'Expected 1 space after logical operator; 0 found';
            $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'NoSpaceAfter');
            if ($fix === true) {
                $phpcsFile->fixer->addContent($stackPtr, ' ');
            }
        } else {
            $next = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 1), null, true);
            if ($tokens[$stackPtr]['line'] === $tokens[$next]['line']
                && strlen($tokens[($stackPtr + 1)]['content']) !== 1
            ) {
                $found = strlen($tokens[($stackPtr + 1)]['content']);
                $error = 'Expected 1 space after logical operator; %s found';
                $data  = array($found);
                $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'TooMuchSpaceAfter', $data);
                if ($fix === true) {
                    $phpcsFile->fixer->replaceToken(($stackPtr + 1), ' ');
                }
            }
        }

    }//end process()


}//end class
