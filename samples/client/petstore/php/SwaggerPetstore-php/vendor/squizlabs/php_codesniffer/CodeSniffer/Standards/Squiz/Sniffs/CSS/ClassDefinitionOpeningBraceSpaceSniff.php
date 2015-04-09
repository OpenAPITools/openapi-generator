<?php
/**
 * Squiz_Sniffs_CSS_ClassDefinitionOpeningBraceSpaceSniff.
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
 * Squiz_Sniffs_CSS_ClassDefinitionOpeningBraceSpaceSniff.
 *
 * Ensure there is a single space before the opening brace in a class definition
 * and the content starts on the next line.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Squiz_Sniffs_CSS_ClassDefinitionOpeningBraceSpaceSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of tokenizers this sniff supports.
     *
     * @var array
     */
    public $supportedTokenizers = array('CSS');


    /**
     * Returns the token types that this sniff is interested in.
     *
     * @return int[]
     */
    public function register()
    {
        return array(T_OPEN_CURLY_BRACKET);

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

        if ($tokens[($stackPtr - 1)]['code'] !== T_WHITESPACE) {
            $error = 'Expected 1 space before opening brace of class definition; 0 found';
            $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'NoneBefore');
            if ($fix === true) {
                $phpcsFile->fixer->addContentBefore($stackPtr, ' ');
            }
        } else {
            $content = $tokens[($stackPtr - 1)]['content'];
            if ($content !== ' ') {
                $length = strlen($content);
                if ($length === 1) {
                    $length = 'tab';
                }

                $error = 'Expected 1 space before opening brace of class definition; %s found';
                $data  = array($length);
                $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'Before', $data);
                if ($fix === true) {
                    $phpcsFile->fixer->replaceToken(($stackPtr - 1), ' ');
                }
            }
        }//end if

        $next = $phpcsFile->findNext(PHP_CodeSniffer_Tokens::$emptyTokens, ($stackPtr + 1), null, true);
        if ($next === false) {
            return;
        }

        // Check for nested class definitions.
        $nested = false;
        $found  = $phpcsFile->findNext(
            T_OPEN_CURLY_BRACKET,
            ($stackPtr + 1),
            $tokens[$stackPtr]['bracket_closer']
        );

        if ($found !== false) {
            $nested = true;
        }

        if ($tokens[$next]['line'] === $tokens[$stackPtr]['line']) {
            $error = 'Opening brace should be the last content on the line';
            $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'ContentBefore');
            if ($fix === true) {
                $phpcsFile->fixer->addNewline($stackPtr);
            }
        } else {
            $foundLines = ($tokens[$next]['line'] - $tokens[$stackPtr]['line'] - 1);
            if ($nested === true) {
                if ($foundLines !== 1) {
                    $error = 'Expected 1 blank line after opening brace of nesting class definition; %s found';
                    $data  = array($foundLines);
                    $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'AfterNesting', $data);

                    if ($fix === true) {
                        if ($foundLines === 0) {
                            $phpcsFile->fixer->addNewline($stackPtr);
                        } else {
                            $next = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 1), null, true);
                            $phpcsFile->fixer->beginChangeset();
                            for ($i = ($stackPtr + 1); $i < ($next + 1); $i++) {
                                $phpcsFile->fixer->replaceToken($i, '');
                            }

                            $phpcsFile->fixer->addNewline($stackPtr);
                            $phpcsFile->fixer->endChangeset();
                        }
                    }
                }//end if
            }//end if
        }//end if

    }//end process()


}//end class
