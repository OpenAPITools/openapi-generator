<?php
/**
 * Reports errors if the same class or interface name is used in multiple files.
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
 * Reports errors if the same class or interface name is used in multiple files.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Generic_Sniffs_Classes_DuplicateClassNameSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * List of classes that have been found during checking.
     *
     * @var array
     */
    public $foundClasses = array();


    /**
     * Registers the tokens that this sniff wants to listen for.
     *
     * @return int[]
     */
    public function register()
    {
        return array(T_OPEN_TAG);

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

        $namespace  = '';
        $findTokens = array(
                       T_CLASS,
                       T_INTERFACE,
                       T_NAMESPACE,
                       T_CLOSE_TAG,
                      );

        $stackPtr = $phpcsFile->findNext($findTokens, ($stackPtr + 1));
        while ($stackPtr !== false) {
            if ($tokens[$stackPtr]['code'] === T_CLOSE_TAG) {
                // We can stop here. The sniff will continue from the next open
                // tag when PHPCS reaches that token, if there is one.
                return;
            }

            // Keep track of what namespace we are in.
            if ($tokens[$stackPtr]['code'] === T_NAMESPACE) {
                $nsEnd = $phpcsFile->findNext(
                    array(
                     T_NS_SEPARATOR,
                     T_STRING,
                     T_WHITESPACE,
                    ),
                    ($stackPtr + 1),
                    null,
                    true
                );

                $namespace = trim($phpcsFile->getTokensAsString(($stackPtr + 1), ($nsEnd - $stackPtr - 1)));
                $stackPtr  = $nsEnd;
            } else {
                $nameToken = $phpcsFile->findNext(T_STRING, $stackPtr);
                $name      = $tokens[$nameToken]['content'];
                if ($namespace !== '') {
                    $name = $namespace.'\\'.$name;
                }

                $compareName = strtolower($name);
                if (isset($this->foundClasses[$compareName]) === true) {
                    $type  = strtolower($tokens[$stackPtr]['content']);
                    $file  = $this->foundClasses[$compareName]['file'];
                    $line  = $this->foundClasses[$compareName]['line'];
                    $error = 'Duplicate %s name "%s" found; first defined in %s on line %s';
                    $data  = array(
                              $type,
                              $name,
                              $file,
                              $line,
                             );
                    $phpcsFile->addWarning($error, $stackPtr, 'Found', $data);
                } else {
                    $this->foundClasses[$compareName] = array(
                                                         'file' => $phpcsFile->getFilename(),
                                                         'line' => $tokens[$stackPtr]['line'],
                                                        );
                }
            }//end if

            $stackPtr = $phpcsFile->findNext($findTokens, ($stackPtr + 1));
        }//end while

    }//end process()


}//end class
