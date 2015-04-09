<?php
/**
 * Ensures that eval() is not used to create objects.
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
 * Ensures that eval() is not used to create objects.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer_MySource
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class MySource_Sniffs_PHP_EvalObjectFactorySniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_EVAL);

    }//end register()


    /**
     * Processes this sniff, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token in
     *                                        the stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        /*
            We need to find all strings that will be in the eval
            to determine if the "new" keyword is being used.
        */

        $openBracket  = $phpcsFile->findNext(T_OPEN_PARENTHESIS, ($stackPtr + 1));
        $closeBracket = $tokens[$openBracket]['parenthesis_closer'];

        $strings = array();
        $vars    = array();

        for ($i = ($openBracket + 1); $i < $closeBracket; $i++) {
            if (isset(PHP_CodeSniffer_Tokens::$stringTokens[$tokens[$i]['code']]) === true) {
                $strings[$i] = $tokens[$i]['content'];
            } else if ($tokens[$i]['code'] === T_VARIABLE) {
                $vars[$i] = $tokens[$i]['content'];
            }
        }

        /*
            We now have some variables that we need to expand into
            the strings that were assigned to them, if any.
        */

        foreach ($vars as $varPtr => $varName) {
            while (($prev = $phpcsFile->findPrevious(T_VARIABLE, ($varPtr - 1))) !== false) {
                // Make sure this is an assignment of the variable. That means
                // it will be the first thing on the line.
                $prevContent = $phpcsFile->findPrevious(T_WHITESPACE, ($prev - 1), null, true);
                if ($tokens[$prevContent]['line'] === $tokens[$prev]['line']) {
                    $varPtr = $prevContent;
                    continue;
                }

                if ($tokens[$prev]['content'] !== $varName) {
                    // This variable has a different name.
                    $varPtr = $prevContent;
                    continue;
                }

                // We found one.
                break;
            }//end while

            if ($prev !== false) {
                // Find all strings on the line.
                $lineEnd = $phpcsFile->findNext(T_SEMICOLON, ($prev + 1));
                for ($i = ($prev + 1); $i < $lineEnd; $i++) {
                    if (isset(PHP_CodeSniffer_Tokens::$stringTokens[$tokens[$i]['code']]) === true) {
                        $strings[$i] = $tokens[$i]['content'];
                    }
                }
            }
        }//end foreach

        foreach ($strings as $string) {
            // If the string has "new" in it, it is not allowed.
            // We don't bother checking if the word "new" is echo'd
            // because that is unlikely to happen. We assume the use
            // of "new" is for object instantiation.
            if (strstr($string, ' new ') !== false) {
                $error = 'Do not use eval() to create objects dynamically; use reflection instead';
                $phpcsFile->addWarning($error, $stackPtr, 'Found');
            }
        }

    }//end process()


}//end class
