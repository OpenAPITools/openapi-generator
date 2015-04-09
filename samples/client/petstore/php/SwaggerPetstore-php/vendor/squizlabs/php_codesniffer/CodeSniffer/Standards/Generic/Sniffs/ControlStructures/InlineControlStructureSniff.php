<?php
/**
 * Generic_Sniffs_ControlStructures_InlineControlStructureSniff.
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
 * Generic_Sniffs_ControlStructures_InlineControlStructureSniff.
 *
 * Verifies that inline control statements are not present.
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
class Generic_Sniffs_ControlStructures_InlineControlStructureSniff implements PHP_CodeSniffer_Sniff
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
     * If true, an error will be thrown; otherwise a warning.
     *
     * @var bool
     */
    public $error = true;


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(
                T_IF,
                T_ELSE,
                T_FOREACH,
                T_WHILE,
                T_DO,
                T_SWITCH,
                T_FOR,
               );

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

        if (isset($tokens[$stackPtr]['scope_opener']) === true) {
            $phpcsFile->recordMetric($stackPtr, 'Control structure defined inline', 'no');
            return;
        }

        // Ignore the ELSE in ELSE IF. We'll process the IF part later.
        if (($tokens[$stackPtr]['code'] === T_ELSE) && ($tokens[($stackPtr + 2)]['code'] === T_IF)) {
            return;
        }

        if ($tokens[$stackPtr]['code'] === T_WHILE) {
            // This could be from a DO WHILE, which doesn't have an opening brace.
            $lastContent = $phpcsFile->findPrevious(T_WHITESPACE, ($stackPtr - 1), null, true);
            if ($tokens[$lastContent]['code'] === T_CLOSE_CURLY_BRACKET) {
                $brace = $tokens[$lastContent];
                if (isset($brace['scope_condition']) === true) {
                    $condition = $tokens[$brace['scope_condition']];
                    if ($condition['code'] === T_DO) {
                        return;
                    }
                }
            }
        }

        // This is a control structure without an opening brace,
        // so it is an inline statement.
        if ($this->error === true) {
            $fix = $phpcsFile->addFixableError('Inline control structures are not allowed', $stackPtr, 'NotAllowed');
        } else {
            $fix = $phpcsFile->addFixableWarning('Inline control structures are discouraged', $stackPtr, 'Discouraged');
        }

        $phpcsFile->recordMetric($stackPtr, 'Control structure defined inline', 'yes');

        if ($fix === true) {
            $phpcsFile->fixer->beginChangeset();
            if (isset($tokens[$stackPtr]['parenthesis_closer']) === true) {
                $closer = $tokens[$stackPtr]['parenthesis_closer'];
            } else {
                $closer = $stackPtr;
            }

            if ($tokens[($closer + 1)]['code'] === T_WHITESPACE
                || $tokens[($closer + 1)]['code'] === T_SEMICOLON
            ) {
                $phpcsFile->fixer->addContent($closer, ' {');
            } else {
                $phpcsFile->fixer->addContent($closer, ' { ');
            }

            $lastNonEmpty = $closer;
            for ($end = ($closer + 1); $end < $phpcsFile->numTokens; $end++) {
                if ($tokens[$end]['code'] === T_SEMICOLON) {
                    break;
                }

                if ($tokens[$end]['code'] === T_CLOSE_TAG) {
                    $end = $lastNonEmpty;
                    break;
                }

                if (isset($tokens[$end]['scope_opener']) === true) {
                    $end = $tokens[$end]['scope_closer'];
                    break;
                }

                if ($tokens[$end]['code'] !== T_WHITESPACE) {
                    $lastNonEmpty = $end;
                }
            }

            $next = $phpcsFile->findNext(T_WHITESPACE, ($closer + 1), ($end + 1), true);

            // Account for a comment on the end of the line.
            for ($endLine = $end; $endLine < $phpcsFile->numTokens; $endLine++) {
                if (isset($tokens[($endLine + 1)]) === false
                    || $tokens[$endLine]['line'] !== $tokens[($endLine + 1)]['line']
                ) {
                    break;
                }
            }

            if ($tokens[$endLine]['code'] !== T_COMMENT) {
                $endLine = $end;
            }

            if ($next !== $end) {
                if ($endLine !== $end) {
                    $phpcsFile->fixer->addContent($endLine, '}');
                } else {
                    if ($tokens[$end]['code'] !== T_SEMICOLON
                        && $tokens[$end]['code'] !== T_CLOSE_CURLY_BRACKET
                    ) {
                        $phpcsFile->fixer->addContent($end, ';');
                    }

                    $phpcsFile->fixer->addContent($end, ' }');
                }
            } else {
                if ($endLine !== $end) {
                    $phpcsFile->fixer->replaceToken($end, '');
                    $phpcsFile->fixer->addNewlineBefore($endLine);
                    $phpcsFile->fixer->addContent($endLine, '}');
                } else {
                    $phpcsFile->fixer->replaceToken($end, '}');
                }
            }//end if

            $phpcsFile->fixer->endChangeset();
        }//end if

    }//end process()


}//end class
