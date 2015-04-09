<?php
/**
 * Squiz_Sniffs_CSS_NamedColoursSniff.
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
 * Squiz_Sniffs_CSS_NamedColoursSniff.
 *
 * Ensure colour names are not used.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Squiz_Sniffs_CSS_NamedColoursSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of tokenizers this sniff supports.
     *
     * @var array
     */
    public $supportedTokenizers = array('CSS');


    /**
     * A list of named colours.
     *
     * This is the list of standard colours defined in the CSS spec.
     *
     * @var array
     */
    public $colourNames = array(
                           'aqua'    => 'aqua',
                           'black'   => 'black',
                           'blue'    => 'blue',
                           'fuchsia' => 'fuchsia',
                           'gray'    => 'gray',
                           'green'   => 'green',
                           'lime'    => 'lime',
                           'maroon'  => 'maroon',
                           'navy'    => 'navy',
                           'olive'   => 'olive',
                           'orange'  => 'orange',
                           'purple'  => 'purple',
                           'red'     => 'red',
                           'silver'  => 'silver',
                           'teal'    => 'teal',
                           'white'   => 'white',
                           'yellow'  => 'yellow',
                          );


    /**
     * Returns the token types that this sniff is interested in.
     *
     * @return int[]
     */
    public function register()
    {
        return array(T_STRING);

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

        if ($tokens[($stackPtr - 1)]['code'] === T_HASH
            || $tokens[($stackPtr - 1)]['code'] === T_STRING_CONCAT
        ) {
            // Class name.
            return;
        }

        if (isset($this->colourNames[strtolower($tokens[$stackPtr]['content'])]) === true) {
            $error = 'Named colours are forbidden; use hex, rgb, or rgba values instead';
            $phpcsFile->addError($error, $stackPtr, 'Forbidden');
        }

    }//end process()


}//end class
