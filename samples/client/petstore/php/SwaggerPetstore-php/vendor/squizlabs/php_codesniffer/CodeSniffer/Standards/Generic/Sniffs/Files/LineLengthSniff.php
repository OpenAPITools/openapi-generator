<?php
/**
 * Generic_Sniffs_Files_LineLengthSniff.
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
 * Generic_Sniffs_Files_LineLengthSniff.
 *
 * Checks all lines in the file, and throws warnings if they are over 80
 * characters in length and errors if they are over 100. Both these
 * figures can be changed by extending this sniff in your own standard.
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
class Generic_Sniffs_Files_LineLengthSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * The limit that the length of a line should not exceed.
     *
     * @var int
     */
    public $lineLimit = 80;

    /**
     * The limit that the length of a line must not exceed.
     *
     * Set to zero (0) to disable.
     *
     * @var int
     */
    public $absoluteLineLimit = 100;


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
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token in
     *                                        the stack passed in $tokens.
     *
     * @return int
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();
        for ($i = 1; $i < $phpcsFile->numTokens; $i++) {
            if ($tokens[$i]['column'] === 1) {
                $this->checkLineLength($phpcsFile, $tokens, $i);
            }
        }

        $this->checkLineLength($phpcsFile, $tokens, $i);

        // Ignore the rest of the file.
        return ($phpcsFile->numTokens + 1);

    }//end process()


    /**
     * Checks if a line is too long.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param array                $tokens    The token stack.
     * @param int                  $stackPtr  The first token on the next line.
     *
     * @return null|false
     */
    protected function checkLineLength(PHP_CodeSniffer_File $phpcsFile, $tokens, $stackPtr)
    {
        // The passed token is the first on the line.
        $stackPtr--;

        if ($tokens[$stackPtr]['column'] === 1
            && $tokens[$stackPtr]['length'] === 0
        ) {
            // Blank line.
            return;
        }

        if ($tokens[$stackPtr]['column'] !== 1
            && $tokens[$stackPtr]['content'] === $phpcsFile->eolChar
        ) {
            $stackPtr--;
        }

        $lineLength = ($tokens[$stackPtr]['column'] + $tokens[$stackPtr]['length'] - 1);

        // Record metrics for common line length groupings.
        if ($lineLength <= 80) {
            $phpcsFile->recordMetric($stackPtr, 'Line length', '80 or less');
        } else if ($lineLength <= 120) {
            $phpcsFile->recordMetric($stackPtr, 'Line length', '81-120');
        } else if ($lineLength <= 150) {
            $phpcsFile->recordMetric($stackPtr, 'Line length', '121-150');
        } else {
            $phpcsFile->recordMetric($stackPtr, 'Line length', '151 or more');
        }

        if ($this->absoluteLineLimit > 0
            && $lineLength > $this->absoluteLineLimit
        ) {
            $data = array(
                     $this->absoluteLineLimit,
                     $lineLength,
                    );

            $error = 'Line exceeds maximum limit of %s characters; contains %s characters';
            $phpcsFile->addError($error, $stackPtr, 'MaxExceeded', $data);
        } else if ($lineLength > $this->lineLimit) {
            $data = array(
                     $this->lineLimit,
                     $lineLength,
                    );

            $warning = 'Line exceeds %s characters; contains %s characters';
            $phpcsFile->addWarning($warning, $stackPtr, 'TooLong', $data);
        }

    }//end checkLineLength()


}//end class
