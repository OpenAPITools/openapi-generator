<?php
/**
 * Squiz_Sniffs_PHP_EmbeddedPhpSniff.
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
 * Squiz_Sniffs_PHP_EmbeddedPhpSniff.
 *
 * Checks the indentation of embedded PHP code segments.
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
class Squiz_Sniffs_PHP_EmbeddedPhpSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_OPEN_TAG);

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

        // If the close php tag is on the same line as the opening
        // then we have an inline embedded PHP block.
        $closeTag = $phpcsFile->findNext(T_CLOSE_TAG, $stackPtr);
        if ($closeTag === false || $tokens[$stackPtr]['line'] !== $tokens[$closeTag]['line']) {
            $this->_validateMultilineEmbeddedPhp($phpcsFile, $stackPtr);
        } else {
            $this->_validateInlineEmbeddedPhp($phpcsFile, $stackPtr);
        }

    }//end process()


    /**
     * Validates embedded PHP that exists on multiple lines.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token in the
     *                                        stack passed in $tokens.
     *
     * @return void
     */
    private function _validateMultilineEmbeddedPhp(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        $prevTag = $phpcsFile->findPrevious(T_OPEN_TAG, ($stackPtr - 1));
        if ($prevTag === false) {
            // This is the first open tag.
            return;
        }

        $firstContent = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 1), null, true);
        $closingTag   = $phpcsFile->findNext(T_CLOSE_TAG, $stackPtr);
        if ($closingTag !== false) {
            $nextContent = $phpcsFile->findNext(T_WHITESPACE, ($closingTag + 1), $phpcsFile->numTokens, true);
            if ($nextContent === false) {
                // Final closing tag. It will be handled elsewhere.
                return;
            }

            // We have an opening and a closing tag, that lie within other content.
            if ($firstContent === $closingTag) {
                $error = 'Empty embedded PHP tag found';
                $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'Empty');
                if ($fix === true) {
                    $phpcsFile->fixer->beginChangeset();
                    for ($i = $stackPtr; $i <= $closingTag; $i++) {
                        $phpcsFile->fixer->replaceToken($i, '');
                    }

                    $phpcsFile->fixer->endChangeset();
                }

                return;
            }
        }//end if

        if ($tokens[$firstContent]['line'] === $tokens[$stackPtr]['line']) {
            $error = 'Opening PHP tag must be on a line by itself';
            $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'ContentAfterOpen');
            if ($fix === true) {
                $first   = $phpcsFile->findFirstOnLine(T_WHITESPACE, $stackPtr, true);
                $padding = (strlen($tokens[$first]['content']) - strlen(ltrim($tokens[$first]['content'])));
                $phpcsFile->fixer->beginChangeset();
                $phpcsFile->fixer->addNewline($stackPtr);
                $phpcsFile->fixer->addContent($stackPtr, str_repeat(' ', $padding));
                $phpcsFile->fixer->endChangeset();
            }
        } else {
            // Check the indent of the first line, except if it is a scope closer.
            if (isset($tokens[$firstContent]['scope_closer']) === false
                || $tokens[$firstContent]['scope_closer'] !== $firstContent
            ) {
                // Check for a blank line at the top.
                if ($tokens[$firstContent]['line'] > ($tokens[$stackPtr]['line'] + 1)) {
                    // Find a token on the blank line to throw the error on.
                    $i = $stackPtr;
                    do {
                        $i++;
                    } while ($tokens[$i]['line'] !== ($tokens[$stackPtr]['line'] + 1));

                    $error = 'Blank line found at start of embedded PHP content';
                    $fix   = $phpcsFile->addFixableError($error, $i, 'SpacingBefore');
                    if ($fix === true) {
                        $phpcsFile->fixer->beginChangeset();
                        for ($i = ($stackPtr + 1); $i < $firstContent; $i++) {
                            if ($tokens[$i]['line'] === $tokens[$firstContent]['line']
                                || $tokens[$i]['line'] === $tokens[$stackPtr]['line']
                            ) {
                                continue;
                            }

                            $phpcsFile->fixer->replaceToken($i, '');
                        }

                        $phpcsFile->fixer->endChangeset();
                    }
                }//end if

                $first = $phpcsFile->findFirstOnLine(T_WHITESPACE, $stackPtr);
                if ($first === false) {
                    $first  = $phpcsFile->findFirstOnLine(T_INLINE_HTML, $stackPtr);
                    $indent = (strlen($tokens[$first]['content']) - strlen(ltrim($tokens[$first]['content'])));
                } else {
                    $indent = ($tokens[($first + 1)]['column'] - 1);
                }

                $contentColumn = ($tokens[$firstContent]['column'] - 1);
                if ($contentColumn !== $indent) {
                    $error = 'First line of embedded PHP code must be indented %s spaces; %s found';
                    $data  = array(
                              $indent,
                              $contentColumn,
                             );
                    $fix   = $phpcsFile->addFixableError($error, $firstContent, 'Indent', $data);
                    if ($fix === true) {
                        $padding = str_repeat(' ', $indent);
                        if ($contentColumn === 0) {
                            $phpcsFile->fixer->addContentBefore($firstContent, $padding);
                        } else {
                            $phpcsFile->fixer->replaceToken(($firstContent - 1), $padding);
                        }
                    }
                }
            }//end if
        }//end if

        $lastContent = $phpcsFile->findPrevious(T_WHITESPACE, ($stackPtr - 1), null, true);
        if ($tokens[$lastContent]['line'] === $tokens[$stackPtr]['line']
            && trim($tokens[$lastContent]['content']) !== ''
        ) {
            $error = 'Opening PHP tag must be on a line by itself';
            $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'ContentBeforeOpen');
            if ($fix === true) {
                $first = $phpcsFile->findFirstOnLine(T_WHITESPACE, $stackPtr);
                if ($first === false) {
                    $first   = $phpcsFile->findFirstOnLine(T_INLINE_HTML, $stackPtr);
                    $padding = (strlen($tokens[$first]['content']) - strlen(ltrim($tokens[$first]['content'])));
                } else {
                    $padding = ($tokens[($first + 1)]['column'] - 1);
                }

                $phpcsFile->fixer->addContentBefore($stackPtr, $phpcsFile->eolChar.str_repeat(' ', $padding));
            }
        } else {
            // Find the first token on the first non-empty line we find.
            for ($first = ($stackPtr - 1); $first > 0; $first--) {
                if ($tokens[$first]['line'] === $tokens[$stackPtr]['line']) {
                    continue;
                } else if (trim($tokens[$first]['content']) !== '') {
                    $first = $phpcsFile->findFirstOnLine(array(), $first, true);
                    break;
                }
            }

            $expected = 0;
            if ($tokens[$first]['code'] === T_INLINE_HTML
                && trim($tokens[$first]['content']) !== ''
            ) {
                $expected = (strlen($tokens[$first]['content']) - strlen(ltrim($tokens[$first]['content'])));
            } else if ($tokens[$first]['code'] === T_WHITESPACE) {
                $expected = ($tokens[($first + 1)]['column'] - 1);
            }

            $expected += 4;
            $found     = ($tokens[$stackPtr]['column'] - 1);
            if ($found > $expected) {
                $error = 'Opening PHP tag indent incorrect; expected no more than %s spaces but found %s';
                $data  = array(
                          $expected,
                          $found,
                         );
                $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'OpenTagIndent', $data);
                if ($fix === true) {
                    $phpcsFile->fixer->replaceToken(($stackPtr - 1), str_repeat(' ', $expected));
                }
            }
        }//end if

        if ($closingTag === false) {
            return;
        }

        $lastContent = $phpcsFile->findPrevious(T_WHITESPACE, ($closingTag - 1), ($stackPtr + 1), true);
        $nextContent = $phpcsFile->findNext(T_WHITESPACE, ($closingTag + 1), null, true);

        if ($tokens[$lastContent]['line'] === $tokens[$closingTag]['line']) {
            $error = 'Closing PHP tag must be on a line by itself';
            $fix   = $phpcsFile->addFixableError($error, $closingTag, 'ContentBeforeEnd');
            if ($fix === true) {
                $first = $phpcsFile->findFirstOnLine(T_WHITESPACE, $closingTag, true);
                $phpcsFile->fixer->beginChangeset();
                $phpcsFile->fixer->addContentBefore($closingTag, str_repeat(' ', ($tokens[$first]['column'] - 1)));
                $phpcsFile->fixer->addNewlineBefore($closingTag);
                $phpcsFile->fixer->endChangeset();
            }
        } else if ($tokens[$nextContent]['line'] === $tokens[$closingTag]['line']) {
            $error = 'Closing PHP tag must be on a line by itself';
            $fix   = $phpcsFile->addFixableError($error, $closingTag, 'ContentAfterEnd');
            if ($fix === true) {
                $first = $phpcsFile->findFirstOnLine(T_WHITESPACE, $closingTag, true);
                $phpcsFile->fixer->beginChangeset();
                $phpcsFile->fixer->addNewline($closingTag);
                $phpcsFile->fixer->addContent($closingTag, str_repeat(' ', ($tokens[$first]['column'] - 1)));
                $phpcsFile->fixer->endChangeset();
            }
        }//end if

        $next = $phpcsFile->findNext(T_OPEN_TAG, ($closingTag + 1));
        if ($next === false) {
            return;
        }

        // Check for a blank line at the bottom.
        if ((isset($tokens[$lastContent]['scope_closer']) === false
            || $tokens[$lastContent]['scope_closer'] !== $lastContent)
            && $tokens[$lastContent]['line'] < ($tokens[$closingTag]['line'] - 1)
        ) {
            // Find a token on the blank line to throw the error on.
            $i = $closingTag;
            do {
                $i--;
            } while ($tokens[$i]['line'] !== ($tokens[$closingTag]['line'] - 1));

            $error = 'Blank line found at end of embedded PHP content';
            $fix   = $phpcsFile->addFixableError($error, $i, 'SpacingAfter');
            if ($fix === true) {
                $phpcsFile->fixer->beginChangeset();
                for ($i = ($lastContent + 1); $i < $closingTag; $i++) {
                    if ($tokens[$i]['line'] === $tokens[$lastContent]['line']
                        || $tokens[$i]['line'] === $tokens[$closingTag]['line']
                    ) {
                        continue;
                    }

                    $phpcsFile->fixer->replaceToken($i, '');
                }

                $phpcsFile->fixer->endChangeset();
            }
        }//end if

    }//end _validateMultilineEmbeddedPhp()


    /**
     * Validates embedded PHP that exists on one line.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token in the
     *                                        stack passed in $tokens.
     *
     * @return void
     */
    private function _validateInlineEmbeddedPhp(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        // We only want one line PHP sections, so return if the closing tag is
        // on the next line.
        $closeTag = $phpcsFile->findNext(T_CLOSE_TAG, $stackPtr, null, false);
        if ($tokens[$stackPtr]['line'] !== $tokens[$closeTag]['line']) {
            return;
        }

        // Check that there is one, and only one space at the start of the statement.
        $firstContent = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 1), ($closeTag - 1), true);

        if ($firstContent === false) {
            $error = 'Empty embedded PHP tag found';
            $fix   = $phpcsFile->addError($error, $stackPtr, 'Empty');
            if ($fix === true) {
                $phpcsFile->fixer->beginChangeset();
                for ($i = $stackPtr; $i <= $closeTag; $i++) {
                    $phpcsFile->fixer->replaceToken($i, '');
                }

                $phpcsFile->fixer->endChangeset();
            }

            return;
        }

        // The open tag token always contains a single space after it.
        $leadingSpace = 1;
        if ($tokens[($stackPtr + 1)]['code'] === T_WHITESPACE) {
            $leadingSpace = (strlen($tokens[($stackPtr + 1)]['content']) + 1);
        }

        if ($leadingSpace !== 1) {
            $error = 'Expected 1 space after opening PHP tag; %s found';
            $data  = array($leadingSpace);
            $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'SpacingAfterOpen', $data);
            if ($fix === true) {
                $phpcsFile->fixer->replaceToken(($stackPtr + 1), '');
            }
        }

        $prev = $phpcsFile->findPrevious(PHP_CodeSniffer_Tokens::$emptyTokens, ($closeTag - 1), $stackPtr, true);
        if ($tokens[$prev]['code'] !== T_SEMICOLON) {
            $error = 'Inline PHP statement must end with a semicolon';
            $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'NoSemicolon');
            if ($fix === true) {
                $phpcsFile->fixer->addContent($prev, ';');
            }
        } else {
            $statementCount = 1;
            for ($i = ($stackPtr + 1); $i < $prev; $i++) {
                if ($tokens[$i]['code'] === T_SEMICOLON) {
                    $statementCount++;
                }
            }

            if ($statementCount > 1) {
                $error = 'Inline PHP statement must contain a single statement; %s found';
                $data  = array($statementCount);
                $phpcsFile->addError($error, $stackPtr, 'MultipleStatements', $data);
            }
        }

        $trailingSpace = 0;
        if ($tokens[($closeTag - 1)]['code'] === T_WHITESPACE) {
            $trailingSpace = strlen($tokens[($closeTag - 1)]['content']);
        }

        if ($trailingSpace !== 1) {
            $error = 'Expected 1 space before closing PHP tag; %s found';
            $data  = array($trailingSpace);
            $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'SpacingBeforeClose', $data);
            if ($fix === true) {
                if ($trailingSpace === 0) {
                    $phpcsFile->fixer->addContentBefore($closeTag, ' ');
                } else {
                    $phpcsFile->fixer->replaceToken(($closeTag - 1), ' ');
                }
            }
        }

    }//end _validateInlineEmbeddedPhp()


}//end class
