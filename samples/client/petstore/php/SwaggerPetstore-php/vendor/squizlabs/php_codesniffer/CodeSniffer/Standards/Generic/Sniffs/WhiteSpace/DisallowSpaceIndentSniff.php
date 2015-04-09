<?php
/**
 * Generic_Sniffs_WhiteSpace_DisallowSpaceIndentSniff.
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
 * Generic_Sniffs_WhiteSpace_DisallowSpaceIndentSniff.
 *
 * Throws errors if spaces are used for indentation.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Generic_Sniffs_WhiteSpace_DisallowSpaceIndentSniff implements PHP_CodeSniffer_Sniff
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
     * The --tab-width CLI value that is being used.
     *
     * @var int
     */
    private $_tabWidth = null;


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
        if ($this->_tabWidth === null) {
            $cliValues = $phpcsFile->phpcs->cli->getCommandLineValues();
            if (isset($cliValues['tabWidth']) === false || $cliValues['tabWidth'] === 0) {
                // We have no idea how wide tabs are, so assume 4 spaces for fixing.
                // It shouldn't really matter because indent checks elsewhere in the
                // standard should fix things up.
                $this->_tabWidth = 4;
            } else {
                $this->_tabWidth = $cliValues['tabWidth'];
            }
        }

        $tokens = $phpcsFile->getTokens();
        for ($i = ($stackPtr + 1); $i < $phpcsFile->numTokens; $i++) {
            if ($tokens[$i]['column'] !== 1
                || ($tokens[$i]['code'] !== T_WHITESPACE
                && $tokens[$i]['code'] !== T_DOC_COMMENT_WHITESPACE
                && $tokens[$i]['code'] !== T_CONSTANT_ENCAPSED_STRING)
            ) {
                continue;
            }

            // If tabs are being converted to spaces, the original content
            // should be used instead of the converted content.
            if (isset($tokens[$i]['orig_content']) === true) {
                $content = $tokens[$i]['orig_content'];
            } else {
                $content = $tokens[$i]['content'];
            }

            if ($content[0] === ' ') {
                if ($tokens[$i]['code'] === T_DOC_COMMENT_WHITESPACE && $content === ' ') {
                    // Ignore file/class-level DocBlock.
                    continue;
                }

                // Space are considered ok if they are proceeded by tabs and not followed
                // by tabs, as is the case with standard docblock comments.
                $phpcsFile->recordMetric($i, 'Line indent', 'spaces');
                $error = 'Tabs must be used to indent lines; spaces are not allowed';
                $fix   = $phpcsFile->addFixableError($error, $i, 'SpacesUsed');
                if ($fix === true) {
                    $trimmed   = ltrim($content, ' ');
                    $numSpaces = (strlen($content) - strlen($trimmed));
                    if ($numSpaces < $this->_tabWidth) {
                        $numTabs = 1;
                        $padding = "\t";
                    } else {
                        $numTabs   = floor($numSpaces / $this->_tabWidth);
                        $remaining = ($numSpaces - ($numTabs * $this->_tabWidth));
                        $padding   = str_repeat("\t", $numTabs).$padding = str_repeat(' ', $remaining);
                    }

                    $phpcsFile->fixer->replaceToken($i, $padding.$trimmed);
                }
            } else if ($content[0] === "\t") {
                $phpcsFile->recordMetric($i, 'Line indent', 'tabs');
            }//end if
        }//end for

        // Ignore the rest of the file.
        return ($phpcsFile->numTokens + 1);

    }//end process()


}//end class
