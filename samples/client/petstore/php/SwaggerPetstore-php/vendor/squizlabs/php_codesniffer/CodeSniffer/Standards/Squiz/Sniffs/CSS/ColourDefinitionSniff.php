<?php
/**
 * Squiz_Sniffs_CSS_ColourDefinitionSniff.
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
 * Squiz_Sniffs_CSS_ColourDefinitionSniff.
 *
 * Ensure colours are defined in upper-case and use shortcuts where possible.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Squiz_Sniffs_CSS_ColourDefinitionSniff implements PHP_CodeSniffer_Sniff
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
        return array(T_COLOUR);

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
        $tokens = $phpcsFile->getTokens();
        $colour = $tokens[$stackPtr]['content'];

        $expected = strtoupper($colour);
        if ($colour !== $expected) {
            $error = 'CSS colours must be defined in uppercase; expected %s but found %s';
            $data  = array(
                      $expected,
                      $colour,
                     );

            $fix = $phpcsFile->addFixableError($error, $stackPtr, 'NotUpper', $data);
            if ($fix === true) {
                $phpcsFile->fixer->replaceToken($stackPtr, $expected);
            }
        }

        // Now check if shorthand can be used.
        if (strlen($colour) !== 7) {
            return;
        }

        if ($colour{1} === $colour{2} && $colour{3} === $colour{4} && $colour{5} === $colour{6}) {
            $expected = '#'.$colour{1}.$colour{3}.$colour{5};
            $error    = 'CSS colours must use shorthand if available; expected %s but found %s';
            $data     = array(
                         $expected,
                         $colour,
                        );

            $fix = $phpcsFile->addFixableError($error, $stackPtr, 'Shorthand', $data);
            if ($fix === true) {
                $phpcsFile->fixer->replaceToken($stackPtr, $expected);
            }
        }

    }//end process()


}//end class
