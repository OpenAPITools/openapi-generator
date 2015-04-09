<?php
/**
 * Squiz_Sniffs_Formatting_OperationBracketSniff.
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
 * Squiz_Sniffs_Formatting_OperationBracketSniff.
 *
 * Tests that all arithmetic operations are bracketed.
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
class Squiz_Sniffs_Formatting_OperatorBracketSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of tokenizers this sniff supports.
     *
     * @var array
     */
    public $supportedTokenizers = array(
                                   'PHP',
                                   'JS',
                                  );


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return PHP_CodeSniffer_Tokens::$operators;

    }//end register()


    /**
     * Processes this test, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token in the
     *                                        stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        if ($phpcsFile->tokenizerType === 'JS' && $tokens[$stackPtr]['code'] === T_PLUS) {
            // JavaScript uses the plus operator for string concatenation as well
            // so we cannot accurately determine if it is a string concat or addition.
            // So just ignore it.
            return;
        }

        // If the & is a reference, then we don't want to check for brackets.
        if ($tokens[$stackPtr]['code'] === T_BITWISE_AND && $phpcsFile->isReference($stackPtr) === true) {
            return;
        }

        // There is one instance where brackets aren't needed, which involves
        // the minus sign being used to assign a negative number to a variable.
        if ($tokens[$stackPtr]['code'] === T_MINUS) {
            // Check to see if we are trying to return -n.
            $prev = $phpcsFile->findPrevious(PHP_CodeSniffer_Tokens::$emptyTokens, ($stackPtr - 1), null, true);
            if ($tokens[$prev]['code'] === T_RETURN) {
                return;
            }

            $number = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 1), null, true);
            if ($tokens[$number]['code'] === T_LNUMBER || $tokens[$number]['code'] === T_DNUMBER) {
                $previous = $phpcsFile->findPrevious(T_WHITESPACE, ($stackPtr - 1), null, true);
                if ($previous !== false) {
                    $isAssignment = in_array($tokens[$previous]['code'], PHP_CodeSniffer_Tokens::$assignmentTokens);
                    $isEquality   = in_array($tokens[$previous]['code'], PHP_CodeSniffer_Tokens::$equalityTokens);
                    $isComparison = in_array($tokens[$previous]['code'], PHP_CodeSniffer_Tokens::$comparisonTokens);
                    if ($isAssignment === true || $isEquality === true || $isComparison === true) {
                        // This is a negative assignment or comparison.
                        // We need to check that the minus and the number are
                        // adjacent.
                        if (($number - $stackPtr) !== 1) {
                            $error = 'No space allowed between minus sign and number';
                            $phpcsFile->addError($error, $stackPtr, 'SpacingAfterMinus');
                        }

                        return;
                    }
                }
            }
        }//end if

        $lastBracket = false;
        if (isset($tokens[$stackPtr]['nested_parenthesis']) === true) {
            $parenthesis = array_reverse($tokens[$stackPtr]['nested_parenthesis'], true);
            foreach ($parenthesis as $bracket => $endBracket) {
                $prevToken = $phpcsFile->findPrevious(T_WHITESPACE, ($bracket - 1), null, true);
                $prevCode  = $tokens[$prevToken]['code'];

                if ($prevCode === T_ISSET) {
                    // This operation is inside an isset() call, but has
                    // no bracket of it's own.
                    break;
                }

                if ($prevCode === T_STRING || $prevCode === T_SWITCH) {
                    // We allow very simple operations to not be bracketed.
                    // For example, ceil($one / $two).
                    $allowed = array(
                                T_VARIABLE,
                                T_LNUMBER,
                                T_DNUMBER,
                                T_STRING,
                                T_WHITESPACE,
                                T_THIS,
                                T_OBJECT_OPERATOR,
                                T_OPEN_SQUARE_BRACKET,
                                T_CLOSE_SQUARE_BRACKET,
                                T_MODULUS,
                               );

                    for ($prev = ($stackPtr - 1); $prev > $bracket; $prev--) {
                        if (in_array($tokens[$prev]['code'], $allowed) === true) {
                            continue;
                        }

                        if ($tokens[$prev]['code'] === T_CLOSE_PARENTHESIS) {
                            $prev = $tokens[$prev]['parenthesis_opener'];
                        } else {
                            break;
                        }
                    }

                    if ($prev !== $bracket) {
                        break;
                    }

                    for ($next = ($stackPtr + 1); $next < $endBracket; $next++) {
                        if (in_array($tokens[$next]['code'], $allowed) === true) {
                            continue;
                        }

                        if ($tokens[$next]['code'] === T_OPEN_PARENTHESIS) {
                            $next = $tokens[$next]['parenthesis_closer'];
                        } else {
                            break;
                        }
                    }

                    if ($next !== $endBracket) {
                        break;
                    }
                }//end if

                if (in_array($prevCode, PHP_CodeSniffer_Tokens::$scopeOpeners) === true) {
                    // This operation is inside a control structure like FOREACH
                    // or IF, but has no bracket of it's own.
                    // The only control structure allowed to do this is SWITCH.
                    if ($prevCode !== T_SWITCH) {
                        break;
                    }
                }

                if ($prevCode === T_OPEN_PARENTHESIS) {
                    // These are two open parenthesis in a row. If the current
                    // one doesn't enclose the operator, go to the previous one.
                    if ($endBracket < $stackPtr) {
                        continue;
                    }
                }

                $lastBracket = $bracket;
                break;
            }//end foreach
        }//end if

        if ($lastBracket === false) {
            // It is not in a bracketed statement at all.
            $previousToken = $phpcsFile->findPrevious(T_WHITESPACE, ($stackPtr - 1), null, true, null, true);
            if ($previousToken !== false) {
                // A list of tokens that indicate that the token is not
                // part of an arithmetic operation.
                $invalidTokens = array(
                                  T_COMMA,
                                  T_COLON,
                                  T_OPEN_PARENTHESIS,
                                  T_OPEN_SQUARE_BRACKET,
                                  T_CASE,
                                 );

                if (in_array($tokens[$previousToken]['code'], $invalidTokens) === false) {
                    $this->addMissingBracketsError($phpcsFile, $stackPtr);
                }

                return;
            }
        } else if ($tokens[$lastBracket]['parenthesis_closer'] < $stackPtr) {
            // There are a set of brackets in front of it that don't include it.
            $this->addMissingBracketsError($phpcsFile, $stackPtr);
            return;
        } else {
            // We are enclosed in a set of bracket, so the last thing to
            // check is that we are not also enclosed in square brackets
            // like this: ($array[$index + 1]), which is invalid.
            $brackets = array(
                         T_OPEN_SQUARE_BRACKET,
                         T_CLOSE_SQUARE_BRACKET,
                        );

            $squareBracket = $phpcsFile->findPrevious($brackets, ($stackPtr - 1), $lastBracket);
            if ($squareBracket !== false && $tokens[$squareBracket]['code'] === T_OPEN_SQUARE_BRACKET) {
                $closeSquareBracket = $phpcsFile->findNext($brackets, ($stackPtr + 1));
                if ($closeSquareBracket !== false && $tokens[$closeSquareBracket]['code'] === T_CLOSE_SQUARE_BRACKET) {
                    $this->addMissingBracketsError($phpcsFile, $stackPtr);
                }
            }

            return;
        }//end if

        $lastAssignment = $phpcsFile->findPrevious(PHP_CodeSniffer_Tokens::$assignmentTokens, $stackPtr, null, false, null, true);
        if ($lastAssignment !== false && $lastAssignment > $lastBracket) {
            $this->addMissingBracketsError($phpcsFile, $stackPtr);
        }

    }//end process()


    /**
     * Add and fix the missing brackets error.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token in the
     *                                        stack passed in $tokens.
     *
     * @return void
     */
    public function addMissingBracketsError(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $error = 'Arithmetic operation must be bracketed';
        $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'MissingBrackets');

        if ($fix === false || $phpcsFile->fixer->enabled === false) {
            return;
        }

        $tokens = $phpcsFile->getTokens();

        $allowed = array(
                    T_VARIABLE        => true,
                    T_LNUMBER         => true,
                    T_DNUMBER         => true,
                    T_STRING          => true,
                    T_WHITESPACE      => true,
                    T_THIS            => true,
                    T_OBJECT_OPERATOR => true,
                    T_MODULUS         => true,
                    T_ISSET           => true,
                    T_ARRAY           => true,
                   );

        // Find the first token in the expression.
        for ($before = ($stackPtr - 1); $before > 0; $before--) {
            // Special case for plus operators because we can't tell if they are used
            // for addition or string contact. So assume string concat to be safe.
            if ($phpcsFile->tokenizerType === 'JS' && $tokens[$before]['code'] === T_PLUS) {
                break;
            }

            if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$tokens[$before]['code']]) === true
                || isset(PHP_CodeSniffer_Tokens::$operators[$tokens[$before]['code']]) === true
                || isset(PHP_CodeSniffer_Tokens::$castTokens[$tokens[$before]['code']]) === true
                || isset($allowed[$tokens[$before]['code']]) === true
            ) {
                continue;
            }

            if ($tokens[$before]['code'] === T_CLOSE_PARENTHESIS) {
                $before = $tokens[$before]['parenthesis_opener'];
                continue;
            }

            if ($tokens[$before]['code'] === T_CLOSE_SQUARE_BRACKET) {
                $before = $tokens[$before]['bracket_opener'];
                continue;
            }

            break;
        }//end for

        $before = $phpcsFile->findNext(PHP_CodeSniffer_Tokens::$emptyTokens, ($before + 1), null, true);

        // Find the last token in the expression.
        for ($after = ($stackPtr + 1); $after < $phpcsFile->numTokens; $after++) {
            // Special case for plus operators because we can't tell if they are used
            // for addition or string contact. So assume string concat to be safe.
            if ($phpcsFile->tokenizerType === 'JS' && $tokens[$after]['code'] === T_PLUS) {
                break;
            }

            if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$tokens[$after]['code']]) === true
                || isset(PHP_CodeSniffer_Tokens::$operators[$tokens[$after]['code']]) === true
                || isset(PHP_CodeSniffer_Tokens::$castTokens[$tokens[$after]['code']]) === true
                || isset($allowed[$tokens[$after]['code']]) === true
            ) {
                continue;
            }

            if ($tokens[$after]['code'] === T_OPEN_PARENTHESIS) {
                $after = $tokens[$after]['parenthesis_closer'];
                continue;
            }

            if ($tokens[$after]['code'] === T_OPEN_SQUARE_BRACKET) {
                $after = $tokens[$after]['bracket_closer'];
                continue;
            }

            break;
        }//end for

        $after = $phpcsFile->findPrevious(PHP_CodeSniffer_Tokens::$emptyTokens, ($after - 1), null, true);

        // Can only fix this error if both tokens are available for fixing.
        // Adding one bracket without the other will create parse errors.
        $phpcsFile->fixer->beginChangeset();
        $phpcsFile->fixer->replaceToken($before, '('.$tokens[$before]['content']);
        $phpcsFile->fixer->replaceToken($after, $tokens[$after]['content'].')');
        $phpcsFile->fixer->endChangeset();

    }//end addMissingBracketsError()


}//end class
