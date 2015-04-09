<?php
/**
 * Squiz_Sniffs_Formatting_FunctionSpacingSniff.
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
 * Squiz_Sniffs_WhiteSpace_FunctionSpacingSniff.
 *
 * Checks the separation between methods in a class or interface.
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
class Squiz_Sniffs_WhiteSpace_FunctionSpacingSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * The number of blank lines between functions.
     *
     * @var int
     */
    public $spacing = 2;


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_FUNCTION);

    }//end register()


    /**
     * Processes this sniff when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token
     *                                        in the stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens        = $phpcsFile->getTokens();
        $this->spacing = (int) $this->spacing;

        /*
            Check the number of blank lines
            after the function.
        */

        if (isset($tokens[$stackPtr]['scope_closer']) === false) {
            // Must be an interface method, so the closer is the semicolon.
            $closer = $phpcsFile->findNext(T_SEMICOLON, $stackPtr);
        } else {
            $closer = $tokens[$stackPtr]['scope_closer'];
        }

        // Allow for comments on the same line as the closer.
        for ($nextLineToken = ($closer + 1); $nextLineToken < $phpcsFile->numTokens; $nextLineToken++) {
            if ($tokens[$nextLineToken]['line'] !== $tokens[$closer]['line']) {
                break;
            }
        }

        $foundLines = 0;
        if ($nextLineToken === ($phpcsFile->numTokens - 1)) {
            // We are at the end of the file.
            // Don't check spacing after the function because this
            // should be done by an EOF sniff.
            $foundLines = $this->spacing;
        } else {
            $nextContent = $phpcsFile->findNext(T_WHITESPACE, $nextLineToken, null, true);
            if ($nextContent === false) {
                // We are at the end of the file.
                // Don't check spacing after the function because this
                // should be done by an EOF sniff.
                $foundLines = $this->spacing;
            } else {
                $foundLines += ($tokens[$nextContent]['line'] - $tokens[$nextLineToken]['line']);
            }
        }

        if ($foundLines !== $this->spacing) {
            $error = 'Expected %s blank line';
            if ($this->spacing !== 1) {
                $error .= 's';
            }

            $error .= ' after function; %s found';
            $data   = array(
                       $this->spacing,
                       $foundLines,
                      );

            $fix = $phpcsFile->addFixableError($error, $closer, 'After', $data);
            if ($fix === true) {
                $phpcsFile->fixer->beginChangeset();
                for ($i = $nextLineToken; $i <= $nextContent; $i++) {
                    if ($tokens[$i]['line'] === $tokens[$nextContent]['line']) {
                        $phpcsFile->fixer->addContentBefore($i, str_repeat($phpcsFile->eolChar, $this->spacing));
                        break;
                    }

                    $phpcsFile->fixer->replaceToken($i, '');
                }

                $phpcsFile->fixer->endChangeset();
            }//end if
        }//end if

        /*
            Check the number of blank lines
            before the function.
        */

        $prevLineToken = null;
        for ($i = $stackPtr; $i > 0; $i--) {
            if (strpos($tokens[$i]['content'], $phpcsFile->eolChar) === false) {
                continue;
            } else {
                $prevLineToken = $i;
                break;
            }
        }

        if (is_null($prevLineToken) === true) {
            // Never found the previous line, which means
            // there are 0 blank lines before the function.
            $foundLines  = 0;
            $prevContent = 0;
        } else {
            $currentLine = $tokens[$stackPtr]['line'];

            $prevContent = $phpcsFile->findPrevious(T_WHITESPACE, $prevLineToken, null, true);
            if ($tokens[$prevContent]['code'] === T_DOC_COMMENT_CLOSE_TAG
                && $tokens[$prevContent]['line'] === ($currentLine - 1)
            ) {
                // Account for function comments.
                $prevContent = $phpcsFile->findPrevious(T_WHITESPACE, ($tokens[$prevContent]['comment_opener'] - 1), null, true);
            }

            // Before we throw an error, check that we are not throwing an error
            // for another function. We don't want to error for no blank lines after
            // the previous function and no blank lines before this one as well.
            $prevLine   = ($tokens[$prevContent]['line'] - 1);
            $i          = ($stackPtr - 1);
            $foundLines = 0;
            while ($currentLine !== $prevLine && $currentLine > 1 && $i > 0) {
                if (isset($tokens[$i]['scope_condition']) === true) {
                    $scopeCondition = $tokens[$i]['scope_condition'];
                    if ($tokens[$scopeCondition]['code'] === T_FUNCTION) {
                        // Found a previous function.
                        return;
                    }
                } else if ($tokens[$i]['code'] === T_FUNCTION) {
                    // Found another interface function.
                    return;
                }

                $currentLine = $tokens[$i]['line'];
                if ($currentLine === $prevLine) {
                    break;
                }

                if ($tokens[($i - 1)]['line'] < $currentLine && $tokens[($i + 1)]['line'] > $currentLine) {
                    // This token is on a line by itself. If it is whitespace, the line is empty.
                    if ($tokens[$i]['code'] === T_WHITESPACE) {
                        $foundLines++;
                    }
                }

                $i--;
            }//end while
        }//end if

        if ($foundLines !== $this->spacing) {
            $error = 'Expected %s blank line';
            if ($this->spacing !== 1) {
                $error .= 's';
            }

            $error .= ' before function; %s found';
            $data   = array(
                       $this->spacing,
                       $foundLines,
                      );

            $fix = $phpcsFile->addFixableError($error, $stackPtr, 'Before', $data);
            if ($fix === true) {
                if ($prevContent === 0) {
                    $nextSpace = 0;
                } else {
                    $nextSpace = $phpcsFile->findNext(T_WHITESPACE, ($prevContent + 1), $stackPtr);
                    if ($nextSpace === false) {
                        $nextSpace = ($stackPtr - 1);
                    }
                }

                if ($foundLines < $this->spacing) {
                    $padding = str_repeat($phpcsFile->eolChar, ($this->spacing - $foundLines));
                    $phpcsFile->fixer->addContent($nextSpace, $padding);
                } else {
                    $nextContent = $phpcsFile->findNext(T_WHITESPACE, ($nextSpace + 1), null, true);
                    $phpcsFile->fixer->beginChangeset();
                    for ($i = $nextSpace; $i < ($nextContent - 1); $i++) {
                        $phpcsFile->fixer->replaceToken($i, '');
                    }

                    $phpcsFile->fixer->replaceToken($i, str_repeat($phpcsFile->eolChar, $this->spacing));
                    $phpcsFile->fixer->endChangeset();
                }
            }//end if
        }//end if

    }//end process()


}//end class
