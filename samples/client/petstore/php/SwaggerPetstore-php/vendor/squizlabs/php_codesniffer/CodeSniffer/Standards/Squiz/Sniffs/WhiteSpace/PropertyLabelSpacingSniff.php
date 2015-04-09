<?php
/**
 * Squiz_Sniffs_WhiteSpace_PropertyLabelSpacingSniff.
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
 * Squiz_Sniffs_WhiteSpace_PropertyLabelSpacingSniff.
 *
 * Ensures that the colon in a property or label definition has a single
 * space after it and no space before it.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Squiz_Sniffs_WhiteSpace_PropertyLabelSpacingSniff implements PHP_CodeSniffer_Sniff
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
        return array(
                T_PROPERTY,
                T_LABEL,
               );

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

        $colon = $phpcsFile->findNext(T_COLON, ($stackPtr + 1));

        if ($colon !== ($stackPtr + 1)) {
            $error = 'There must be no space before the colon in a property/label declaration';
            $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'Before');
            if ($fix === true) {
                $phpcsFile->fixer->replaceToken(($stackPtr + 1), '');
            }
        }

        if ($tokens[($colon + 1)]['code'] !== T_WHITESPACE || $tokens[($colon + 1)]['content'] !== ' ') {
            $error = 'There must be a single space after the colon in a property/label declaration';
            $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'After');
            if ($fix === true) {
                if ($tokens[($colon + 1)]['code'] === T_WHITESPACE) {
                    $phpcsFile->fixer->replaceToken(($colon + 1), ' ');
                } else {
                    $phpcsFile->fixer->addContent($colon, ' ');
                }
            }
        }

    }//end process()


}//end class
