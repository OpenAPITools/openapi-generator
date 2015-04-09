<?php
/**
 * MySource_Sniffs_CSS_BrowserSpecificStylesSniff.
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
 * MySource_Sniffs_CSS_BrowserSpecificStylesSniff.
 *
 * Ensure that browser-specific styles are not used.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class MySource_Sniffs_CSS_BrowserSpecificStylesSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of tokenizers this sniff supports.
     *
     * @var array
     */
    public $supportedTokenizers = array('CSS');

    /**
     * A list of specific stylesheet suffixes we allow.
     *
     * These stylesheets contain browser specific styles
     * so this sniff ignore them files in the form:
     * *_moz.css and *_ie7.css etc.
     *
     * @var array
     */
    protected $specificStylesheets = array(
                                      'moz'    => true,
                                      'ie'     => true,
                                      'ie7'    => true,
                                      'ie8'    => true,
                                      'webkit' => true,
                                     );


    /**
     * Returns the token types that this sniff is interested in.
     *
     * @return int[]
     */
    public function register()
    {
        return array(T_STYLE);

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
        // Ignore files with browser-specific suffixes.
        $filename  = $phpcsFile->getFilename();
        $breakChar = strrpos($filename, '_');
        if ($breakChar !== false && substr($filename, -4) === '.css') {
            $specific = substr($filename, ($breakChar + 1), -4);
            if (isset($this->specificStylesheets[$specific]) === true) {
                return;
            }
        }

        $tokens  = $phpcsFile->getTokens();
        $content = $tokens[$stackPtr]['content'];

        if ($content{0} === '-') {
            $error = 'Browser-specific styles are not allowed';
            $phpcsFile->addError($error, $stackPtr, 'ForbiddenStyle');
        }

    }//end process()


}//end class
