<?php
/**
 * Squiz_Sniffs_PHP_InnerFunctionsSniff.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Marc McIntyre <mmcintyre@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Squiz_Sniffs_PHP_NonExecutableCodeSniff.
 *
 * Warns about code that can never been executed. This happens when a function
 * returns before the code, or a break ends execution of a statement etc.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Marc McIntyre <mmcintyre@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Squiz_Sniffs_PHP_NonExecutableCodeSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(
                T_BREAK,
                T_CONTINUE,
                T_RETURN,
                T_THROW,
                T_EXIT,
               );

    }//end register()


    /**
     * Processes this test, when one of its tokens is encountered.
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

        // If this token is preceded with an "or", it only relates to one line
        // and should be ignored. For example: fopen() or die().
        $prev = $phpcsFile->findPrevious(PHP_CodeSniffer_Tokens::$emptyTokens, ($stackPtr - 1), null, true);
        if ($tokens[$prev]['code'] === T_LOGICAL_OR) {
            return;
        }

        // Check if this token is actually part of a one-line IF or ELSE statement.
        for ($i = ($stackPtr - 1); $i > 0; $i--) {
            if ($tokens[$i]['code'] === T_CLOSE_PARENTHESIS) {
                $i = $tokens[$i]['parenthesis_opener'];
                continue;
            } else if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$tokens[$i]['code']]) === true) {
                continue;
            }

            break;
        }

        if ($tokens[$i]['code'] === T_IF
            || $tokens[$i]['code'] === T_ELSE
            || $tokens[$i]['code'] === T_ELSEIF
        ) {
            return;
        }

        if ($tokens[$stackPtr]['code'] === T_RETURN) {
            $next = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 1), null, true);
            if ($tokens[$next]['code'] === T_SEMICOLON) {
                $next = $phpcsFile->findNext(T_WHITESPACE, ($next + 1), null, true);
                if ($tokens[$next]['code'] === T_CLOSE_CURLY_BRACKET) {
                    // If this is the closing brace of a function
                    // then this return statement doesn't return anything
                    // and is not required anyway.
                    $owner = $tokens[$next]['scope_condition'];
                    if ($tokens[$owner]['code'] === T_FUNCTION) {
                        $warning = 'Empty return statement not required here';
                        $phpcsFile->addWarning($warning, $stackPtr, 'ReturnNotRequired');
                        return;
                    }
                }
            }
        }

        if (isset($tokens[$stackPtr]['scope_opener']) === true) {
            $owner = $tokens[$stackPtr]['scope_condition'];
            if ($tokens[$owner]['code'] === T_CASE || $tokens[$owner]['code'] === T_DEFAULT) {
                // This token closes the scope of a CASE or DEFAULT statement
                // so any code between this token and the next CASE, DEFAULT or
                // end of SWITCH token will not be executable.
                $next = $phpcsFile->findNext(
                    array(
                     T_CASE,
                     T_DEFAULT,
                     T_CLOSE_CURLY_BRACKET,
                    ),
                    ($stackPtr + 1)
                );

                if ($next !== false) {
                    $end      = $phpcsFile->findNext(array(T_SEMICOLON), ($stackPtr + 1));
                    $lastLine = $tokens[$end]['line'];
                    for ($i = ($stackPtr + 1); $i < $next; $i++) {
                        if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$tokens[$i]['code']]) === true) {
                            continue;
                        }

                        $line = $tokens[$i]['line'];
                        if ($line > $lastLine) {
                            $type    = substr($tokens[$stackPtr]['type'], 2);
                            $warning = 'Code after %s statement cannot be executed';
                            $data    = array($type);
                            $phpcsFile->addWarning($warning, $i, 'Unreachable', $data);
                            $lastLine = $line;
                        }
                    }
                }//end if

                // That's all we have to check for these types of statements.
                return;
            }//end if
        }//end if

        // This token may be part of an inline condition.
        // If we find a closing parenthesis that belongs to a condition
        // we should ignore this token.
        $prev = $phpcsFile->findPrevious(PHP_CodeSniffer_Tokens::$emptyTokens, ($stackPtr - 1), null, true);
        if (isset($tokens[$prev]['parenthesis_owner']) === true) {
            $owner  = $tokens[$prev]['parenthesis_owner'];
            $ignore = array(
                       T_IF     => true,
                       T_ELSE   => true,
                       T_ELSEIF => true,
                      );
            if (isset($ignore[$tokens[$owner]['code']]) === true) {
                return;
            }
        }

        $ourConditions = array_keys($tokens[$stackPtr]['conditions']);

        if (empty($ourConditions) === false) {
            $condition = array_pop($ourConditions);

            if (isset($tokens[$condition]['scope_closer']) === false) {
                return;
            }

            // Special case for BREAK statements sitting directly inside SWITCH
            // statements. If we get to this point, we know the BREAK is not being
            // used to close a CASE statement, so it is most likely non-executable
            // code itself (as is the case when you put return; break; at the end of
            // a case). So we need to ignore this token.
            if ($tokens[$condition]['code'] === T_SWITCH
                && $tokens[$stackPtr]['code'] === T_BREAK
            ) {
                return;
            }

            $closer = $tokens[$condition]['scope_closer'];

            // If the closer for our condition is shared with other openers,
            // we will need to throw errors from this token to the next
            // shared opener (if there is one), not to the scope closer.
            $nextOpener = null;
            for ($i = ($stackPtr + 1); $i < $closer; $i++) {
                if (isset($tokens[$i]['scope_closer']) === true) {
                    if ($tokens[$i]['scope_closer'] === $closer) {
                        // We found an opener that shares the same
                        // closing token as us.
                        $nextOpener = $i;
                        break;
                    }
                }
            }//end for

            if ($nextOpener === null) {
                $end = $closer;
            } else {
                $end = ($nextOpener - 1);
            }
        } else {
            // This token is in the global scope.
            if ($tokens[$stackPtr]['code'] === T_BREAK) {
                return;
            }

            // Throw an error for all lines until the end of the file.
            $end = ($phpcsFile->numTokens - 1);
        }//end if

        // Find the semicolon that ends this statement, skipping
        // nested statements like FOR loops and closures.
        for ($start = ($stackPtr + 1); $start < $phpcsFile->numTokens; $start++) {
            if ($start === $end) {
                break;
            }

            if ($tokens[$start]['code'] === T_OPEN_PARENTHESIS) {
                $start = $tokens[$start]['parenthesis_closer'];
                continue;
            }

            if ($tokens[$start]['code'] === T_OPEN_CURLY_BRACKET) {
                $start = $tokens[$start]['bracket_closer'];
                continue;
            }

            if ($tokens[$start]['code'] === T_SEMICOLON) {
                break;
            }
        }//end for

        $lastLine = $tokens[$start]['line'];
        for ($i = ($start + 1); $i < $end; $i++) {
            if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$tokens[$i]['code']]) === true
                || isset(PHP_CodeSniffer_Tokens::$bracketTokens[$tokens[$i]['code']]) === true
            ) {
                continue;
            }

            // Skip whole functions and classes/interfaces because they are not
            // technically executed code, but rather declarations that may be used.
            if ($tokens[$i]['code'] === T_FUNCTION
                || $tokens[$i]['code'] === T_CLASS
                || $tokens[$i]['code'] === T_INTERFACE
            ) {
                $i = $tokens[$i]['scope_closer'];
                continue;
            }

            $line = $tokens[$i]['line'];
            if ($line > $lastLine) {
                $type    = substr($tokens[$stackPtr]['type'], 2);
                $warning = 'Code after %s statement cannot be executed';
                $data    = array($type);
                $phpcsFile->addWarning($warning, $i, 'Unreachable', $data);
                $lastLine = $line;
            }
        }//end for

    }//end process()


}//end class
