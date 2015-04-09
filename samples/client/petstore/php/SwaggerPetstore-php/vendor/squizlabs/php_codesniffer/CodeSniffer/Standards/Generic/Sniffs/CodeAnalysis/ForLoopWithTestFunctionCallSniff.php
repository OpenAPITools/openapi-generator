<?php
/**
 * This file is part of the CodeAnalysis add-on for PHP_CodeSniffer.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Manuel Pichler <mapi@manuel-pichler.de>
 * @copyright 2007-2014 Manuel Pichler. All rights reserved.
 * @license   http://www.opensource.org/licenses/bsd-license.php  BSD License
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Detects for-loops that use a function call in the test expression.
 *
 * This rule is based on the PMD rule catalog. Detects for-loops that use a
 * function call in the test expression.
 *
 * <code>
 * class Foo
 * {
 *     public function bar($x)
 *     {
 *         $a = array(1, 2, 3, 4);
 *         for ($i = 0; $i < count($a); $i++) {
 *              $a[$i] *= $i;
 *         }
 *     }
 * }
 * </code>
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Manuel Pichler <mapi@manuel-pichler.de>
 * @copyright 2007-2014 Manuel Pichler. All rights reserved.
 * @license   http://www.opensource.org/licenses/bsd-license.php BSD License
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Generic_Sniffs_CodeAnalysis_ForLoopWithTestFunctionCallSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Registers the tokens that this sniff wants to listen for.
     *
     * @return int[]
     */
    public function register()
    {
        return array(T_FOR);

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
        $token  = $tokens[$stackPtr];

        // Skip invalid statement.
        if (isset($token['parenthesis_opener']) === false) {
            return;
        }

        $next = ++$token['parenthesis_opener'];
        $end  = --$token['parenthesis_closer'];

        $position = 0;

        for (; $next <= $end; ++$next) {
            $code = $tokens[$next]['code'];
            if ($code === T_SEMICOLON) {
                ++$position;
            }

            if ($position < 1) {
                continue;
            } else if ($position > 1) {
                break;
            } else if ($code !== T_VARIABLE && $code !== T_STRING) {
                continue;
            }

            // Find next non empty token, if it is a open curly brace we have a
            // function call.
            $index = $phpcsFile->findNext(PHP_CodeSniffer_Tokens::$emptyTokens, ($next + 1), null, true);

            if ($tokens[$index]['code'] === T_OPEN_PARENTHESIS) {
                $error = 'Avoid function calls in a FOR loop test part';
                $phpcsFile->addWarning($error, $stackPtr, 'NotAllowed');
                break;
            }
        }//end for

    }//end process()


}//end class
