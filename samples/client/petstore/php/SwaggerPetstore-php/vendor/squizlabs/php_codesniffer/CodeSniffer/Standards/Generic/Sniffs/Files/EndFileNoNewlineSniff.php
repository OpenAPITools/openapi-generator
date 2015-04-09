<?php
/**
 * Generic_Sniffs_Files_EndFileNoNewlineSniff.
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
 * Generic_Sniffs_Files_EndFileNoNewlineSniff.
 *
 * Ensures the file does not end with a newline character.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Generic_Sniffs_Files_EndFileNoNewlineSniff implements PHP_CodeSniffer_Sniff
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
     * Processes this sniff, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token in
     *                                        the stack passed in $tokens.
     *
     * @return int
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        // Skip to the end of the file.
        $tokens   = $phpcsFile->getTokens();
        $stackPtr = ($phpcsFile->numTokens - 1);

        if ($phpcsFile->tokenizerType !== 'PHP') {
            $stackPtr--;
        }

        $eolCharLen = strlen($phpcsFile->eolChar);
        $lastChars  = substr($tokens[$stackPtr]['content'], ($eolCharLen * -1));
        if ($lastChars === $phpcsFile->eolChar) {
            $error = 'File must not end with a newline character';
            $fix   = $phpcsFile->addFixableError($error, $stackPtr, 'Found');
            if ($fix === true) {
                $newContent = substr($tokens[$stackPtr]['content'], 0, ($eolCharLen * -1));
                $phpcsFile->fixer->replaceToken($stackPtr, $newContent);
            }
        }

        // Ignore the rest of the file.
        return ($phpcsFile->numTokens + 1);

    }//end process()


}//end class
