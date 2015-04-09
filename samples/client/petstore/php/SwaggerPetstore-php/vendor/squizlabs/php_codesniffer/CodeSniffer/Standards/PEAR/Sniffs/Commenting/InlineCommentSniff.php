<?php
/**
 * PHP_CodeSniffer_Sniffs_PEAR_Commenting_InlineCommentSniff.
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
 * PHP_CodeSniffer_Sniffs_PEAR_Commenting_InlineCommentSniff.
 *
 * Checks that no perl-style comments are used.
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
class PEAR_Sniffs_Commenting_InlineCommentSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_COMMENT);

    }//end register()


    /**
     * Processes this test, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token
     *                                        in the stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        if ($tokens[$stackPtr]['content']{0} === '#') {
            $phpcsFile->recordMetric($stackPtr, 'Inline comment style', '# ...');

            $error  = 'Perl-style comments are not allowed. Use "// Comment."';
            $error .= ' or "/* comment */" instead.';
            $fix    = $phpcsFile->addFixableError($error, $stackPtr, 'WrongStyle');
            if ($fix === true) {
                $newComment = ltrim($tokens[$stackPtr]['content'], '# ');
                $newComment = '// '.$newComment;
                $phpcsFile->fixer->replaceToken($stackPtr, $newComment);
            }
        } else if ($tokens[$stackPtr]['content']{0} === '/'
            && $tokens[$stackPtr]['content']{1} === '/'
        ) {
            $phpcsFile->recordMetric($stackPtr, 'Inline comment style', '// ...');
        } else if ($tokens[$stackPtr]['content']{0} === '/'
            && $tokens[$stackPtr]['content']{1} === '*'
        ) {
            $phpcsFile->recordMetric($stackPtr, 'Inline comment style', '/* ... */');
        }

    }//end process()


}//end class
