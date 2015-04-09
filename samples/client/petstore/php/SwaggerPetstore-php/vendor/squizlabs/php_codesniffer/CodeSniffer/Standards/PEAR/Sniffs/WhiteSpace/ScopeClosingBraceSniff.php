<?php
/**
 * PEAR_Sniffs_Whitespace_ScopeClosingBraceSniff.
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
 * PEAR_Sniffs_Whitespace_ScopeClosingBraceSniff.
 *
 * Checks that the closing braces of scopes are aligned correctly.
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
class PEAR_Sniffs_WhiteSpace_ScopeClosingBraceSniff implements PHP_CodeSniffer_Sniff
{

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
        return PHP_CodeSniffer_Tokens::$scopeOpeners;

    }//end register()


    /**
     * Processes this test, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile All the tokens found in the document.
     * @param int                  $stackPtr  The position of the current token
     *                                        in the stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        // If this is an inline condition (ie. there is no scope opener), then
        // return, as this is not a new scope.
        if (isset($tokens[$stackPtr]['scope_closer']) === false) {
            return;
        }

        $scopeStart = $tokens[$stackPtr]['scope_opener'];
        $scopeEnd   = $tokens[$stackPtr]['scope_closer'];

        // If the scope closer doesn't think it belongs to this scope opener
        // then the opener is sharing its closer ith other tokens. We only
        // want to process the closer once, so skip this one.
        if ($tokens[$scopeEnd]['scope_condition'] !== $stackPtr) {
            return;
        }

        // We need to actually find the first piece of content on this line,
        // because if this is a method with tokens before it (public, static etc)
        // or an if with an else before it, then we need to start the scope
        // checking from there, rather than the current token.
        $lineStart = ($stackPtr - 1);
        for ($lineStart; $lineStart > 0; $lineStart--) {
            if (strpos($tokens[$lineStart]['content'], $phpcsFile->eolChar) !== false) {
                break;
            }
        }

        // We found a new line, now go forward and find the first non-whitespace
        // token.
        $lineStart = $phpcsFile->findNext(
            array(T_WHITESPACE),
            ($lineStart + 1),
            null,
            true
        );

        $startColumn = $tokens[$lineStart]['column'];

        // Check that the closing brace is on it's own line.
        $lastContent = $phpcsFile->findPrevious(
            array(T_WHITESPACE),
            ($scopeEnd - 1),
            $scopeStart,
            true
        );

        if ($tokens[$lastContent]['line'] === $tokens[$scopeEnd]['line']) {
            $error = 'Closing brace must be on a line by itself';
            $fix   = $phpcsFile->addFixableError($error, $scopeEnd, 'Line');
            if ($fix === true) {
                $phpcsFile->fixer->addNewlineBefore($scopeEnd);
            }

            return;
        }

        // Check now that the closing brace is lined up correctly.
        $fix         = false;
        $braceIndent = ($tokens[$scopeEnd]['column'] - 1);
        if ($tokens[$stackPtr]['code'] === T_CASE
            || $tokens[$stackPtr]['code'] === T_DEFAULT
        ) {
            // BREAK statements should be indented n spaces from the
            // CASE or DEFAULT statement.
            $expectedIndent = ($startColumn + $this->indent - 1);
            if ($braceIndent !== $expectedIndent) {
                $error = 'Case breaking statement indented incorrectly; expected %s spaces, found %s';
                $data  = array(
                          $expectedIndent,
                          $braceIndent,
                         );
                $fix   = $phpcsFile->addFixableError($error, $scopeEnd, 'BreakIdent', $data);
            }
        } else {
            $expectedIndent = ($startColumn - 1);
            if ($braceIndent !== $expectedIndent) {
                $error = 'Closing brace indented incorrectly; expected %s spaces, found %s';
                $data  = array(
                          $expectedIndent,
                          $braceIndent,
                         );
                $fix   = $phpcsFile->addFixableError($error, $scopeEnd, 'Indent', $data);
            }
        }//end if

        if ($fix === true && $phpcsFile->fixer->enabled === true) {
            $spaces = str_repeat(' ', $expectedIndent);
            if ($braceIndent === 0) {
                $phpcsFile->fixer->addContentBefore($scopeEnd, $spaces);
            } else {
                $phpcsFile->fixer->replaceToken(($scopeEnd - 1), $spaces);
            }
        }

    }//end process()


}//end class
