<?php
/**
 * Squiz_Sniffs_CSS_IndentationSniff.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Squiz_Sniffs_CSS_IndentationSniff.
 *
 * Ensures styles are indented 4 spaces.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Squiz_Sniffs_CSS_IndentationSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of tokenizers this sniff supports.
     *
     * @var array
     */
    public $supportedTokenizers = array('CSS');

    /**
     * The number of spaces code should be indented.
     *
     * @var int
     */
    public $indent = 4;


    /**
     * Returns the token types that this sniff is interested in.
     *
     * @return int[]
     */
    public function register()
    {
        return array(T_OPEN_TAG);

    }//end register()


    /**
     * Processes the tokens that this sniff is interested in.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file where the token was found.
     * @param int                  $stackPtr  The position in the stack where
     *                                        the token was found.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        $numTokens    = (count($tokens) - 2);
        $indentLevel  = 0;
        $nestingLevel = 0;
        for ($i = 1; $i < $numTokens; $i++) {
            if ($tokens[$i]['code'] === T_COMMENT) {
                // Don't check the indent of comments.
                continue;
            }

            if ($tokens[$i]['code'] === T_OPEN_CURLY_BRACKET) {
                $indentLevel++;

                // Check for nested class definitions.
                $found = $phpcsFile->findNext(
                    T_OPEN_CURLY_BRACKET,
                    ($i + 1),
                    $tokens[$i]['bracket_closer']
                );

                if ($found !== false) {
                    $nestingLevel = $indentLevel;
                }
            }

            if (($tokens[$i]['code'] === T_CLOSE_CURLY_BRACKET
                && $tokens[$i]['line'] !== $tokens[($i - 1)]['line'])
                || ($tokens[($i + 1)]['code'] === T_CLOSE_CURLY_BRACKET
                && $tokens[$i]['line'] === $tokens[($i + 1)]['line'])
            ) {
                $indentLevel--;
                if ($indentLevel === 0) {
                    $nestingLevel = 0;
                }
            }

            if ($tokens[$i]['column'] !== 1) {
                continue;
            }

            // We started a new line, so check indent.
            if ($tokens[$i]['code'] === T_WHITESPACE) {
                $content     = str_replace($phpcsFile->eolChar, '', $tokens[$i]['content']);
                $foundIndent = strlen($content);
            } else {
                $foundIndent = 0;
            }

            $expectedIndent = ($indentLevel * $this->indent);
            if ($expectedIndent > 0
                && strpos($tokens[$i]['content'], $phpcsFile->eolChar) !== false
            ) {
                if ($nestingLevel !== $indentLevel) {
                    $error = 'Blank lines are not allowed in class definitions';
                    $fix   = $phpcsFile->addFixableError($error, $i, 'BlankLine');
                    if ($fix === true) {
                        $phpcsFile->fixer->replaceToken($i, '');
                    }
                }
            } else if ($foundIndent !== $expectedIndent) {
                $error = 'Line indented incorrectly; expected %s spaces, found %s';
                $data  = array(
                          $expectedIndent,
                          $foundIndent,
                         );

                $fix = $phpcsFile->addFixableError($error, $i, 'Incorrect', $data);
                if ($fix === true) {
                    $indent = str_repeat(' ', $expectedIndent);
                    if ($foundIndent === 0) {
                        $phpcsFile->fixer->addContentBefore($i, $indent);
                    } else {
                        $phpcsFile->fixer->replaceToken($i, $indent);
                    }
                }
            }//end if
        }//end for

    }//end process()


}//end class
