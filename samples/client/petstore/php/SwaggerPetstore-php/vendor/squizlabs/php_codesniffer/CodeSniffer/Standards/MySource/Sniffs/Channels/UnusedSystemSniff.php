<?php
/**
 * Ensures that systems, asset types and libs are included before they are used.
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
 * Ensures that systems and asset types are used if they are included.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer_MySource
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class MySource_Sniffs_Channels_UnusedSystemSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_DOUBLE_COLON);

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

        // Check if this is a call to includeSystem, includeAsset or includeWidget.
        $methodName = strtolower($tokens[($stackPtr + 1)]['content']);
        if ($methodName === 'includesystem'
            || $methodName === 'includeasset'
            || $methodName === 'includewidget'
        ) {
            $systemName = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 3), null, true);
            if ($systemName === false || $tokens[$systemName]['code'] !== T_CONSTANT_ENCAPSED_STRING) {
                // Must be using a variable instead of a specific system name.
                // We can't accurately check that.
                return;
            }

            $systemName = trim($tokens[$systemName]['content'], " '");
        } else {
            return;
        }

        if ($methodName === 'includeasset') {
            $systemName .= 'assettype';
        } else if ($methodName === 'includewidget') {
            $systemName .= 'widgettype';
        }

        $systemName = strtolower($systemName);

        // Now check if this system is used anywhere in this scope.
        $level = $tokens[$stackPtr]['level'];
        for ($i = ($stackPtr + 1); $i < $phpcsFile->numTokens; $i++) {
            if ($tokens[$i]['level'] < $level) {
                // We have gone out of scope.
                // If the original include was inside an IF statement that
                // is checking if the system exists, check the outer scope
                // as well.
                if ($tokens[$stackPtr]['level'] === $level) {
                    // We are still in the base level, so this is the first
                    // time we have got here.
                    $conditions = array_keys($tokens[$stackPtr]['conditions']);
                    if (empty($conditions) === false) {
                        $cond = array_pop($conditions);
                        if ($tokens[$cond]['code'] === T_IF) {
                            $i = $tokens[$cond]['scope_closer'];
                            $level--;
                            continue;
                        }
                    }
                }

                break;
            }//end if

            if ($tokens[$i]['code'] !== T_DOUBLE_COLON
                && $tokens[$i]['code'] !== T_EXTENDS
                && $tokens[$i]['code'] !== T_IMPLEMENTS
            ) {
                continue;
            }

            switch ($tokens[$i]['code']) {
            case T_DOUBLE_COLON:
                $usedName = strtolower($tokens[($i - 1)]['content']);
                if ($usedName === $systemName) {
                    // The included system was used, so it is fine.
                    return;
                }
                break;
            case T_EXTENDS:
                $classNameToken = $phpcsFile->findNext(T_STRING, ($i + 1));
                $className      = strtolower($tokens[$classNameToken]['content']);
                if ($className === $systemName) {
                    // The included system was used, so it is fine.
                    return;
                }
                break;
            case T_IMPLEMENTS:
                $endImplements = $phpcsFile->findNext(array(T_EXTENDS, T_OPEN_CURLY_BRACKET), ($i + 1));
                for ($x = ($i + 1); $x < $endImplements; $x++) {
                    if ($tokens[$x]['code'] === T_STRING) {
                        $className = strtolower($tokens[$x]['content']);
                        if ($className === $systemName) {
                            // The included system was used, so it is fine.
                            return;
                        }
                    }
                }
                break;
            }//end switch
        }//end for

        // If we get to here, the system was not use.
        $error = 'Included system "%s" is never used';
        $data  = array($systemName);
        $phpcsFile->addError($error, $stackPtr, 'Found', $data);

    }//end process()


}//end class
