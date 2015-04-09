<?php
/**
 * Generic_Sniffs_WhiteSpace_DisallowTabIndentSniff.
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
 * Generic_Sniffs_WhiteSpace_DisallowTabIndentSniff.
 *
 * Throws errors if tabs are used for indentation.
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
class Generic_Sniffs_WhiteSpace_DisallowTabIndentSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of tokenizers this sniff supports.
     *
     * @var array
     */
    public $supportedTokenizers = array(
                                   'PHP',
                                   'JS',
                                   'CSS',
                                  );


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
     * @param PHP_CodeSniffer_File $phpcsFile All the tokens found in the document.
     * @param int                  $stackPtr  The position of the current token in
     *                                        the stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens    = $phpcsFile->getTokens();
        $error     = 'Spaces must be used to indent lines; tabs are not allowed';
        $errorCode = 'TabsUsed';

        $checkTokens = array(
                        T_WHITESPACE             => true,
                        T_DOC_COMMENT_WHITESPACE => true,
                        T_DOC_COMMENT_STRING     => true,
                       );

        for ($i = ($stackPtr + 1); $i < $phpcsFile->numTokens; $i++) {
            if (isset($checkTokens[$tokens[$i]['code']]) === false) {
                continue;
            }

            // If tabs are being converted to spaces by PHPCS, the
            // original content should be checked instead of the converted content.
            if (isset($tokens[$i]['orig_content']) === true) {
                $content = $tokens[$i]['orig_content'];
            } else {
                $content = $tokens[$i]['content'];
            }

            if ($content === '') {
                continue;
            }

            if ($tokens[$i]['code'] === T_DOC_COMMENT_WHITESPACE && $content === ' ') {
                // Ignore file/class-level DocBlock, especially for recording metrics.
                continue;
            }

            $tabFound = false;
            if ($tokens[$i]['column'] === 1) {
                if ($content[0] === "\t") {
                    $phpcsFile->recordMetric($i, 'Line indent', 'tabs');
                    $tabFound = true;
                } else if ($content[0] === ' ') {
                    if (strpos($content, "\t") !== false) {
                        $phpcsFile->recordMetric($i, 'Line indent', 'mixed');
                        $tabFound = true;
                    } else {
                        $phpcsFile->recordMetric($i, 'Line indent', 'spaces');
                    }
                }
            } else {
                // Look for tabs so we can report and replace, but don't
                // record any metrics about them because they aren't
                // line indent tokens.
                if (strpos($content, "\t") !== false) {
                    $tabFound  = true;
                    $error     = 'Spaces must be used for alignment; tabs are not allowed';
                    $errorCode = 'NonIndentTabsUsed';
                }
            }//end if

            if ($tabFound === false) {
                continue;
            }

            $fix = $phpcsFile->addFixableError($error, $i, $errorCode);
            if ($fix === true) {
                if (isset($tokens[$i]['orig_content']) === true) {
                    // Use the replacement that PHPCS has already done.
                    $phpcsFile->fixer->replaceToken($i, $tokens[$i]['content']);
                } else {
                    // Replace tabs with spaces, using an indent of 4 spaces.
                    // Other sniffs can then correct the indent if they need to.
                    $newContent = str_replace("\t", '    ', $tokens[$i]['content']);
                    $phpcsFile->fixer->replaceToken($i, $newContent);
                }
            }
        }//end for

        // Ignore the rest of the file.
        return ($phpcsFile->numTokens + 1);

    }//end process()


}//end class
