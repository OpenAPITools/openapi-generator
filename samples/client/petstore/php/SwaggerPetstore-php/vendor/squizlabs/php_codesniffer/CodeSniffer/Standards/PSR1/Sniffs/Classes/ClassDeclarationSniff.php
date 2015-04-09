<?php
/**
 * Class Declaration Test.
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
 * Class Declaration Test.
 *
 * Checks the declaration of the class is correct.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PSR1_Sniffs_Classes_ClassDeclarationSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(
                T_CLASS,
                T_INTERFACE,
                T_TRAIT,
               );

    }//end register()


    /**
     * Processes this test, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param integer              $stackPtr  The position of the current token in
     *                                        the token stack.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();
        if (isset($tokens[$stackPtr]['scope_closer']) === false) {
            return;
        }

        $errorData = array(strtolower($tokens[$stackPtr]['content']));

        $nextClass = $phpcsFile->findNext(array(T_CLASS, T_INTERFACE, T_TRAIT), ($tokens[$stackPtr]['scope_closer'] + 1));
        if ($nextClass !== false) {
            $error = 'Each %s must be in a file by itself';
            $phpcsFile->addError($error, $nextClass, 'MultipleClasses', $errorData);
            $phpcsFile->recordMetric($stackPtr, 'One class per file', 'no');
        } else {
            $phpcsFile->recordMetric($stackPtr, 'One class per file', 'yes');
        }

        if (version_compare(PHP_VERSION, '5.3.0') >= 0) {
            $namespace = $phpcsFile->findNext(array(T_NAMESPACE, T_CLASS, T_INTERFACE, T_TRAIT), 0);
            if ($tokens[$namespace]['code'] !== T_NAMESPACE) {
                $error = 'Each %s must be in a namespace of at least one level (a top-level vendor name)';
                $phpcsFile->addError($error, $stackPtr, 'MissingNamespace', $errorData);
                $phpcsFile->recordMetric($stackPtr, 'Class defined in namespace', 'no');
            } else {
                $phpcsFile->recordMetric($stackPtr, 'Class defined in namespace', 'yes');
            }
        }

    }//end process()


}//end class
