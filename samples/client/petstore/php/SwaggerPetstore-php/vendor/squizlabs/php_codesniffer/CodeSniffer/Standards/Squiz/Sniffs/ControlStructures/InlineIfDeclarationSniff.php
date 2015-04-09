<?php
/**
 * Squiz_Sniffs_ControlStructures_InlineControlStructureSniff.
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
 * Squiz_Sniffs_ControlStructures_InlineIfDeclarationSniff.
 *
 * Tests the spacing of shorthand IF statements.
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
class Squiz_Sniffs_ControlStructures_InlineIfDeclarationSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_INLINE_THEN);

    }//end register()


    /**
     * Processes this sniff, when one of its tokens is encountered.
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

        $openBracket  = null;
        $closeBracket = null;
        if (isset($tokens[$stackPtr]['nested_parenthesis']) === true) {
            $parens       = $tokens[$stackPtr]['nested_parenthesis'];
            $openBracket  = array_pop($parens);
            $closeBracket = $tokens[$openBracket]['parenthesis_closer'];
        }

        // Find the beginning of the statement. If we don't find a
        // semicolon (end of statement) or comma (end of array value)
        // then assume the content before the closing parenthesis is the end.
        $else         = $phpcsFile->findNext(T_INLINE_ELSE, ($stackPtr + 1));
        $statementEnd = $phpcsFile->findNext(array(T_SEMICOLON, T_COMMA), ($else + 1), $closeBracket);
        if ($statementEnd === false) {
            $statementEnd = $phpcsFile->findPrevious(T_WHITESPACE, ($closeBracket - 1), null, true);
        }

        // Make sure it's all on the same line.
        if ($tokens[$statementEnd]['line'] !== $tokens[$stackPtr]['line']) {
            $error = 'Inline shorthand IF statement must be declared on a single line';
            $phpcsFile->addError($error, $stackPtr, 'NotSingleLine');
            return;
        }

        // Make sure there are spaces around the question mark.
        $contentBefore = $phpcsFile->findPrevious(T_WHITESPACE, ($stackPtr - 1), null, true);
        $contentAfter  = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 1), null, true);
        if ($tokens[$contentBefore]['code'] !== T_CLOSE_PARENTHESIS) {
            $error = 'Inline shorthand IF statement requires brackets around comparison';
            $phpcsFile->addError($error, $stackPtr, 'NoBrackets');
            return;
        }

        $spaceBefore = ($tokens[$stackPtr]['column'] - ($tokens[$contentBefore]['column'] + strlen($tokens[$contentBefore]['content'])));
        if ($spaceBefore !== 1) {
            $error = 'Inline shorthand IF statement requires 1 space before THEN; %s found';
            $data  = array($spaceBefore);
            $phpcsFile->addError($error, $stackPtr, 'SpacingBeforeThen', $data);
        }

        $spaceAfter = (($tokens[$contentAfter]['column']) - ($tokens[$stackPtr]['column'] + 1));
        if ($spaceAfter !== 1) {
            $error = 'Inline shorthand IF statement requires 1 space after THEN; %s found';
            $data  = array($spaceAfter);
            $phpcsFile->addError($error, $stackPtr, 'SpacingAfterThen', $data);
        }

        // Make sure the ELSE has the correct spacing.
        $inlineElse    = $phpcsFile->findNext(T_INLINE_ELSE, ($stackPtr + 1), $statementEnd, false);
        $contentBefore = $phpcsFile->findPrevious(T_WHITESPACE, ($inlineElse - 1), null, true);
        $contentAfter  = $phpcsFile->findNext(T_WHITESPACE, ($inlineElse + 1), null, true);

        $spaceBefore = ($tokens[$inlineElse]['column'] - ($tokens[$contentBefore]['column'] + strlen($tokens[$contentBefore]['content'])));
        if ($spaceBefore !== 1) {
            $error = 'Inline shorthand IF statement requires 1 space before ELSE; %s found';
            $data  = array($spaceBefore);
            $phpcsFile->addError($error, $inlineElse, 'SpacingBeforeElse', $data);
        }

        $spaceAfter = (($tokens[$contentAfter]['column']) - ($tokens[$inlineElse]['column'] + 1));
        if ($spaceAfter !== 1) {
            $error = 'Inline shorthand IF statement requires 1 space after ELSE; %s found';
            $data  = array($spaceAfter);
            $phpcsFile->addError($error, $inlineElse, 'SpacingAfterElse', $data);
        }

    }//end process()


}//end class
