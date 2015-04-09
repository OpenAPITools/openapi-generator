<?php
/**
 * Squiz_Sniffs_CSS_LowercaseStyleDefinitionSniff.
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
 * Squiz_Sniffs_CSS_LowercaseStyleDefinitionSniff.
 *
 * Ensure that all style definitions are in lowercase.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Squiz_Sniffs_CSS_LowercaseStyleDefinitionSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of tokenizers this sniff supports.
     *
     * @var array
     */
    public $supportedTokenizers = array('CSS');


    /**
     * Returns the token types that this sniff is interested in.
     *
     * @return int[]
     */
    public function register()
    {
        return array(T_OPEN_CURLY_BRACKET);

    }//end register()


    /**
     * Processes the tokens that this sniff is interested in.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file where the token was found.
     * @param int                  $stackPtr  The position in the stack where
     *                                        the token was found.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens  = $phpcsFile->getTokens();
        $start   = ($stackPtr + 1);
        $end     = ($tokens[$stackPtr]['bracket_closer'] - 1);
        $inStyle = null;

        for ($i = $start; $i <= $end; $i++) {
            // Skip nested definitions as they are checked individually.
            if ($tokens[$i]['code'] === T_OPEN_CURLY_BRACKET) {
                $i = $tokens[$i]['bracket_closer'];
                continue;
            }

            if ($tokens[$i]['code'] === T_STYLE) {
                $inStyle = $tokens[$i]['content'];
            }

            if ($tokens[$i]['code'] === T_SEMICOLON) {
                $inStyle = null;
            }

            if ($inStyle === 'progid') {
                // Special case for IE filters.
                continue;
            }

            if ($tokens[$i]['code'] === T_STYLE
                || ($inStyle !== null
                && $tokens[$i]['code'] === T_STRING)
            ) {
                $expected = strtolower($tokens[$i]['content']);
                if ($expected !== $tokens[$i]['content']) {
                    $error = 'Style definitions must be lowercase; expected %s but found %s';
                    $data  = array(
                              $expected,
                              $tokens[$i]['content'],
                             );

                    $fix = $phpcsFile->addFixableError($error, $i, 'FoundUpper', $data);
                    if ($fix === true) {
                        $phpcsFile->fixer->replaceToken($i, $expected);
                    }
                }
            }
        }//end for

    }//end process()


}//end class
