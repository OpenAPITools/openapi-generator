<?php
/**
 * Squiz_Sniffs_ControlStructures_LongConditionClosingCommentSniff.
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
 * Squiz_Sniffs_ControlStructures_LongConditionClosingCommentSniff.
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
class Squiz_Sniffs_Commenting_LongConditionClosingCommentSniff implements PHP_CodeSniffer_Sniff
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
     * The openers that we are interested in.
     *
     * @var array(int)
     */
    private static $_openers = array(
                                T_SWITCH,
                                T_IF,
                                T_FOR,
                                T_FOREACH,
                                T_WHILE,
                                T_TRY,
                                T_CASE,
                               );

    /**
     * The length that a code block must be before
     * requiring a closing comment.
     *
     * @var int
     */
    protected $lineLimit = 20;


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_CLOSE_CURLY_BRACKET);

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

        if (isset($tokens[$stackPtr]['scope_condition']) === false) {
            // No scope condition. It is a function closer.
            return;
        }

        $startCondition = $tokens[$tokens[$stackPtr]['scope_condition']];
        $startBrace     = $tokens[$tokens[$stackPtr]['scope_opener']];
        $endBrace       = $tokens[$stackPtr];

        // We are only interested in some code blocks.
        if (in_array($startCondition['code'], self::$_openers) === false) {
            return;
        }

        if ($startCondition['code'] === T_IF) {
            // If this is actually and ELSE IF, skip it as the brace
            // will be checked by the original IF.
            $else = $phpcsFile->findPrevious(T_WHITESPACE, ($tokens[$stackPtr]['scope_condition'] - 1), null, true);
            if ($tokens[$else]['code'] === T_ELSE) {
                return;
            }

            // IF statements that have an ELSE block need to use
            // "end if" rather than "end else" or "end elseif".
            do {
                $nextToken = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 1), null, true);
                if ($tokens[$nextToken]['code'] === T_ELSE || $tokens[$nextToken]['code'] === T_ELSEIF) {
                    // Check for ELSE IF (2 tokens) as opposed to ELSEIF (1 token).
                    if ($tokens[$nextToken]['code'] === T_ELSE
                        && isset($tokens[$nextToken]['scope_closer']) === false
                    ) {
                        $nextToken = $phpcsFile->findNext(T_WHITESPACE, ($nextToken + 1), null, true);
                        if ($tokens[$nextToken]['code'] !== T_IF
                            || isset($tokens[$nextToken]['scope_closer']) === false
                        ) {
                            // Not an ELSE IF or is an inline ELSE IF.
                            break;
                        }
                    }

                    if (isset($tokens[$nextToken]['scope_closer']) === false) {
                        // There isn't going to be anywhere to print the "end if" comment
                        // because there is no closer.
                        return;
                    }

                    // The end brace becomes the ELSE's end brace.
                    $stackPtr = $tokens[$nextToken]['scope_closer'];
                    $endBrace = $tokens[$stackPtr];
                } else {
                    break;
                }//end if
            } while (isset($tokens[$nextToken]['scope_closer']) === true);
        }//end if

        if ($startCondition['code'] === T_TRY) {
            // TRY statements need to check until the end of all CATCH statements.
            do {
                $nextToken = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 1), null, true);
                if ($tokens[$nextToken]['code'] === T_CATCH) {
                    // The end brace becomes the CATCH's end brace.
                    $stackPtr = $tokens[$nextToken]['scope_closer'];
                    $endBrace = $tokens[$stackPtr];
                } else {
                    break;
                }
            } while (isset($tokens[$nextToken]['scope_closer']) === true);
        }

        $lineDifference = ($endBrace['line'] - $startBrace['line']);

        $expected = '//end '.$startCondition['content'];
        $comment  = $phpcsFile->findNext(array(T_COMMENT), $stackPtr, null, false);

        if (($comment === false) || ($tokens[$comment]['line'] !== $endBrace['line'])) {
            if ($lineDifference >= $this->lineLimit) {
                $error = 'End comment for long condition not found; expected "%s"';
                $data  = array($expected);
                $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'Missing', $data);

                if ($fix === true) {
                    $next = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 1), null, true);
                    if ($next !== false && $tokens[$next]['line'] === $tokens[$stackPtr]['line']) {
                        $expected .= $phpcsFile->eolChar;
                    }

                    $phpcsFile->fixer->addContent($stackPtr, $expected);
                }
            }

            return;
        }

        if (($comment - $stackPtr) !== 1) {
            $error = 'Space found before closing comment; expected "%s"';
            $data  = array($expected);
            $phpcsFile->addError($error, $stackPtr, 'SpacingBefore', $data);
        }

        if (trim($tokens[$comment]['content']) !== $expected) {
            $found = trim($tokens[$comment]['content']);
            $error = 'Incorrect closing comment; expected "%s" but found "%s"';
            $data  = array(
                      $expected,
                      $found,
                     );

            $fix = $phpcsFile->addFixableError($error, $stackPtr, 'Invalid', $data);
            if ($fix === true) {
                $phpcsFile->fixer->replaceToken($comment, $expected.$phpcsFile->eolChar);
            }

            return;
        }

    }//end process()


}//end class
