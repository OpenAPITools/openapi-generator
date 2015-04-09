<?php
/**
 * Squiz_Sniffs_Classes_DuplicatePropertySniff.
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
 * Squiz_Sniffs_Classes_DuplicatePropertySniff.
 *
 * Ensures JS classes don't contain duplicate property names.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Squiz_Sniffs_Classes_DuplicatePropertySniff implements PHP_CodeSniffer_Sniff
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
        return array(T_OBJECT);

    }//end register()


    /**
     * Processes this test, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The current file being processed.
     * @param int                  $stackPtr  The position of the current token in the
     *                                        stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        $properties   = array();
        $wantedTokens = array(
                         T_PROPERTY,
                         T_OBJECT,
                        );

        $next = $phpcsFile->findNext($wantedTokens, ($stackPtr + 1), $tokens[$stackPtr]['bracket_closer']);
        while ($next !== false && $next < $tokens[$stackPtr]['bracket_closer']) {
            if ($tokens[$next]['code'] === T_OBJECT) {
                // Skip nested objects.
                $next = $tokens[$next]['bracket_closer'];
            } else {
                $propName = $tokens[$next]['content'];
                if (isset($properties[$propName]) === true) {
                    $error = 'Duplicate property definition found for "%s"; previously defined on line %s';
                    $data  = array(
                              $propName,
                              $tokens[$properties[$propName]]['line'],
                             );
                    $phpcsFile->addError($error, $next, 'Found', $data);
                }

                $properties[$propName] = $next;
            }//end if

            $next = $phpcsFile->findNext($wantedTokens, ($next + 1), $tokens[$stackPtr]['bracket_closer']);
        }//end while

    }//end process()


}//end class
