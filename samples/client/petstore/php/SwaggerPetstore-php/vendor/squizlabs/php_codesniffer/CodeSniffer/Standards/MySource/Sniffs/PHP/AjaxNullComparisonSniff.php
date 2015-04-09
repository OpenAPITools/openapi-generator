<?php
/**
 * Ensures that values submitted via JS are not compared to NULL.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer_MySource
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Ensures that values submitted via JS are not compared to NULL.
 *
 * With jQuery 1.8, the behaviour of ajax requests changed so that null values are
 * submitted as null= instead of null=null.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer_MySource
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class MySource_Sniffs_PHP_AjaxNullComparisonSniff implements PHP_CodeSniffer_Sniff
{


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
     * Processes this sniff, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token in
     *                                        the stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        // Make sure it is an API function. We know this by the doc comment.
        $commentEnd   = $phpcsFile->findPrevious(T_DOC_COMMENT_CLOSE_TAG, $stackPtr);
        $commentStart = $phpcsFile->findPrevious(T_DOC_COMMENT_OPEN_TAG, ($commentEnd - 1));
        $comment      = $phpcsFile->getTokensAsString($commentStart, ($commentEnd - $commentStart));
        if (strpos($comment, '* @api') === false) {
            return;
        }

        // Find all the vars passed in as we are only interested in comparisons
        // to NULL for these specific variables.
        $foundVars = array();
        $open      = $tokens[$stackPtr]['parenthesis_opener'];
        $close     = $tokens[$stackPtr]['parenthesis_closer'];
        for ($i = ($open + 1); $i < $close; $i++) {
            if ($tokens[$i]['code'] === T_VARIABLE) {
                $foundVars[$tokens[$i]['content']] = true;
            }
        }

        if (empty($foundVars) === true) {
            return;
        }

        $start = $tokens[$stackPtr]['scope_opener'];
        $end   = $tokens[$stackPtr]['scope_closer'];
        for ($i = ($start + 1); $i < $end; $i++) {
            if ($tokens[$i]['code'] !== T_VARIABLE
                || isset($foundVars[$tokens[$i]['content']]) === false
            ) {
                continue;
            }

            $operator = $phpcsFile->findNext(T_WHITESPACE, ($i + 1), null, true);
            if ($tokens[$operator]['code'] !== T_IS_IDENTICAL
                && $tokens[$operator]['code'] !== T_IS_NOT_IDENTICAL
            ) {
                continue;
            }

            $nullValue = $phpcsFile->findNext(T_WHITESPACE, ($operator + 1), null, true);
            if ($tokens[$nullValue]['code'] !== T_NULL) {
                continue;
            }

            $error = 'Values submitted via Ajax requests should not be compared directly to NULL; use empty() instead';
            $phpcsFile->addWarning($error, $nullValue, 'Found');
        }//end for

    }//end process()


}//end class
