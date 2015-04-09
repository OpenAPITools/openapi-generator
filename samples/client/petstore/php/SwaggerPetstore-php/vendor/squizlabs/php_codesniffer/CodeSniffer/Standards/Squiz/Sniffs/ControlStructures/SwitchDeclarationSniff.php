<?php
/**
 * Squiz_Sniffs_ControlStructures_SwitchDeclarationSniff.
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
 * Squiz_Sniffs_ControlStructures_SwitchDeclarationSniff.
 *
 * Ensures all the breaks and cases are aligned correctly according to their
 * parent switch's alignment and enforces other switch formatting.
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
class Squiz_Sniffs_ControlStructures_SwitchDeclarationSniff implements PHP_CodeSniffer_Sniff
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
     * The number of spaces code should be indented.
     *
     * @var int
     */
    public $indent = 4;


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_SWITCH);

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

        // We can't process SWITCH statements unless we know where they start and end.
        if (isset($tokens[$stackPtr]['scope_opener']) === false
            || isset($tokens[$stackPtr]['scope_closer']) === false
        ) {
            return;
        }

        $switch        = $tokens[$stackPtr];
        $nextCase      = $stackPtr;
        $caseAlignment = ($switch['column'] + $this->indent);
        $caseCount     = 0;
        $foundDefault  = false;

        while (($nextCase = $phpcsFile->findNext(array(T_CASE, T_DEFAULT, T_SWITCH), ($nextCase + 1), $switch['scope_closer'])) !== false) {
            // Skip nested SWITCH statements; they are handled on their own.
            if ($tokens[$nextCase]['code'] === T_SWITCH) {
                $nextCase = $tokens[$nextCase]['scope_closer'];
                continue;
            }

            if ($tokens[$nextCase]['code'] === T_DEFAULT) {
                $type         = 'Default';
                $foundDefault = true;
            } else {
                $type = 'Case';
                $caseCount++;
            }

            if ($tokens[$nextCase]['content'] !== strtolower($tokens[$nextCase]['content'])) {
                $expected = strtolower($tokens[$nextCase]['content']);
                $error    = strtoupper($type).' keyword must be lowercase; expected "%s" but found "%s"';
                $data     = array(
                             $expected,
                             $tokens[$nextCase]['content'],
                            );

                $fix = $phpcsFile->addFixableError($error, $nextCase, $type.'NotLower', $data);
                if ($fix === true) {
                    $phpcsFile->fixer->replaceToken($nextCase, $expected);
                }
            }

            if ($tokens[$nextCase]['column'] !== $caseAlignment) {
                $error = strtoupper($type).' keyword must be indented '.$this->indent.' spaces from SWITCH keyword';
                $fix   = $phpcsFile->addFixableError($error, $nextCase, $type.'Indent');

                if ($fix === true) {
                    $padding = str_repeat(' ', ($caseAlignment - 1));
                    if ($tokens[$nextCase]['column'] === 1
                        || $tokens[($nextCase - 1)]['code'] !== T_WHITESPACE
                    ) {
                        $phpcsFile->fixer->addContentBefore($nextCase, $padding);
                    } else {
                        $phpcsFile->fixer->replaceToken(($nextCase - 1), $padding);
                    }
                }
            }

            if ($type === 'Case'
                && ($tokens[($nextCase + 1)]['type'] !== 'T_WHITESPACE'
                || $tokens[($nextCase + 1)]['content'] !== ' ')
            ) {
                $error = 'CASE keyword must be followed by a single space';
                $fix   = $phpcsFile->addFixableError($error, $nextCase, 'SpacingAfterCase');
                if ($fix === true) {
                    if ($tokens[($nextCase + 1)]['type'] !== 'T_WHITESPACE') {
                        $phpcsFile->fixer->addContent($nextCase, ' ');
                    } else {
                        $phpcsFile->fixer->replaceToken(($nextCase + 1), ' ');
                    }
                }
            }

            if (isset($tokens[$nextCase]['scope_opener']) === false) {
                $error = 'Possible parse error: CASE missing opening colon';
                $phpcsFile->addWarning($error, $nextCase, 'MissingColon');
                continue;
            }

            $opener = $tokens[$nextCase]['scope_opener'];
            if ($tokens[($opener - 1)]['type'] === 'T_WHITESPACE') {
                $error = 'There must be no space before the colon in a '.strtoupper($type).' statement';
                $fix   = $phpcsFile->addFixableError($error, $nextCase, 'SpaceBeforeColon'.$type);
                if ($fix === true) {
                    $phpcsFile->fixer->replaceToken(($opener - 1), '');
                }
            }

            $nextBreak = $tokens[$nextCase]['scope_closer'];
            if ($tokens[$nextBreak]['code'] === T_BREAK
                || $tokens[$nextBreak]['code'] === T_RETURN
                || $tokens[$nextBreak]['code'] === T_CONTINUE
                || $tokens[$nextBreak]['code'] === T_THROW
                || $tokens[$nextBreak]['code'] === T_EXIT
            ) {
                if ($tokens[$nextBreak]['scope_condition'] === $nextCase) {
                    // Only need to check a couple of things once, even if the
                    // break is shared between multiple case statements, or even
                    // the default case.
                    if ($tokens[$nextBreak]['column'] !== $caseAlignment) {
                        $error = 'Case breaking statement must be indented '.$this->indent.' spaces from SWITCH keyword';
                        $fix   = $phpcsFile->addFixableError($error, $nextBreak, 'BreakIndent');

                        if ($fix === true) {
                            $padding = str_repeat(' ', ($caseAlignment - 1));
                            if ($tokens[$nextBreak]['column'] === 1
                                || $tokens[($nextBreak - 1)]['code'] !== T_WHITESPACE
                            ) {
                                $phpcsFile->fixer->addContentBefore($nextBreak, $padding);
                            } else {
                                $phpcsFile->fixer->replaceToken(($nextBreak - 1), $padding);
                            }
                        }
                    }

                    $prev = $phpcsFile->findPrevious(T_WHITESPACE, ($nextBreak - 1), $stackPtr, true);
                    if ($tokens[$prev]['line'] !== ($tokens[$nextBreak]['line'] - 1)) {
                        $error = 'Blank lines are not allowed before case breaking statements';
                        $phpcsFile->addError($error, $nextBreak, 'SpacingBeforeBreak');
                    }

                    $breakLine = $tokens[$nextBreak]['line'];
                    $nextLine  = $tokens[$tokens[$stackPtr]['scope_closer']]['line'];
                    $semicolon = $phpcsFile->findNext(T_SEMICOLON, $nextBreak);
                    for ($i = ($semicolon + 1); $i < $tokens[$stackPtr]['scope_closer']; $i++) {
                        if ($tokens[$i]['type'] !== 'T_WHITESPACE') {
                            $nextLine = $tokens[$i]['line'];
                            break;
                        }
                    }

                    if ($type === 'Case') {
                        // Ensure the BREAK statement is followed by
                        // a single blank line, or the end switch brace.
                        if ($nextLine !== ($tokens[$semicolon]['line'] + 2) && $i !== $tokens[$stackPtr]['scope_closer']) {
                            $error = 'Case breaking statements must be followed by a single blank line';
                            $fix   = $phpcsFile->addFixableError($error, $nextBreak, 'SpacingAfterBreak');
                            if ($fix === true) {
                                $phpcsFile->fixer->beginChangeset();
                                for ($i = ($semicolon + 1); $i <= $tokens[$stackPtr]['scope_closer']; $i++) {
                                    if ($tokens[$i]['line'] === $nextLine) {
                                        $phpcsFile->fixer->addNewlineBefore($i);
                                        break;
                                    }

                                    if ($tokens[$i]['line'] === $tokens[$semicolon]['line']) {
                                        continue;
                                    }

                                    $phpcsFile->fixer->replaceToken($i, '');
                                }

                                $phpcsFile->fixer->endChangeset();
                            }
                        }//end if
                    } else {
                        // Ensure the BREAK statement is not followed by a blank line.
                        if ($nextLine !== ($breakLine + 1)) {
                            $error = 'Blank lines are not allowed after the DEFAULT case\'s breaking statement';
                            $phpcsFile->addError($error, $nextBreak, 'SpacingAfterDefaultBreak');
                        }
                    }//end if

                    $caseLine = $tokens[$nextCase]['line'];
                    $nextLine = $tokens[$nextBreak]['line'];
                    for ($i = ($opener + 1); $i < $nextBreak; $i++) {
                        if ($tokens[$i]['type'] !== 'T_WHITESPACE') {
                            $nextLine = $tokens[$i]['line'];
                            break;
                        }
                    }

                    if ($nextLine !== ($caseLine + 1)) {
                        $error = 'Blank lines are not allowed after '.strtoupper($type).' statements';
                        $phpcsFile->addError($error, $nextCase, 'SpacingAfter'.$type);
                    }
                }//end if

                if ($tokens[$nextBreak]['code'] === T_BREAK) {
                    if ($type === 'Case') {
                        // Ensure empty CASE statements are not allowed.
                        // They must have some code content in them. A comment is not enough.
                        // But count RETURN statements as valid content if they also
                        // happen to close the CASE statement.
                        $foundContent = false;
                        for ($i = ($tokens[$nextCase]['scope_opener'] + 1); $i < $nextBreak; $i++) {
                            if ($tokens[$i]['code'] === T_CASE) {
                                $i = $tokens[$i]['scope_opener'];
                                continue;
                            }

                            if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$tokens[$i]['code']]) === false) {
                                $foundContent = true;
                                break;
                            }
                        }

                        if ($foundContent === false) {
                            $error = 'Empty CASE statements are not allowed';
                            $phpcsFile->addError($error, $nextCase, 'EmptyCase');
                        }
                    } else {
                        // Ensure empty DEFAULT statements are not allowed.
                        // They must (at least) have a comment describing why
                        // the default case is being ignored.
                        $foundContent = false;
                        for ($i = ($tokens[$nextCase]['scope_opener'] + 1); $i < $nextBreak; $i++) {
                            if ($tokens[$i]['type'] !== 'T_WHITESPACE') {
                                $foundContent = true;
                                break;
                            }
                        }

                        if ($foundContent === false) {
                            $error = 'Comment required for empty DEFAULT case';
                            $phpcsFile->addError($error, $nextCase, 'EmptyDefault');
                        }
                    }//end if
                }//end if
            } else if ($type === 'Default') {
                $error = 'DEFAULT case must have a breaking statement';
                $phpcsFile->addError($error, $nextCase, 'DefaultNoBreak');
            }//end if
        }//end while

        if ($foundDefault === false) {
            $error = 'All SWITCH statements must contain a DEFAULT case';
            $phpcsFile->addError($error, $stackPtr, 'MissingDefault');
        }

        if ($tokens[$switch['scope_closer']]['column'] !== $switch['column']) {
            $error = 'Closing brace of SWITCH statement must be aligned with SWITCH keyword';
            $phpcsFile->addError($error, $switch['scope_closer'], 'CloseBraceAlign');
        }

        if ($caseCount === 0) {
            $error = 'SWITCH statements must contain at least one CASE statement';
            $phpcsFile->addError($error, $stackPtr, 'MissingCase');
        }

    }//end process()


}//end class
