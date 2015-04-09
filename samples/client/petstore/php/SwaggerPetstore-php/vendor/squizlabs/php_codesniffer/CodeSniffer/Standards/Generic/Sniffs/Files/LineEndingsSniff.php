<?php
/**
 * Generic_Sniffs_Files_LineEndingsSniff.
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
 * Generic_Sniffs_Files_LineEndingsSniff.
 *
 * Checks that end of line characters are correct.
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
class Generic_Sniffs_Files_LineEndingsSniff implements PHP_CodeSniffer_Sniff
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
     * The valid EOL character.
     *
     * @var string
     */
    public $eolChar = '\n';


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
        $found = $phpcsFile->eolChar;
        $found = str_replace("\n", '\n', $found);
        $found = str_replace("\r", '\r', $found);

        $phpcsFile->recordMetric($stackPtr, 'EOL char', $found);

        if ($found === $this->eolChar) {
            // Ignore the rest of the file.
            return ($phpcsFile->numTokens + 1);
        }

        // Check for single line files without an EOL. This is a very special
        // case and the EOL char is set to \n when this happens.
        if ($found === '\n') {
            $tokens    = $phpcsFile->getTokens();
            $lastToken = ($phpcsFile->numTokens - 1);
            if ($tokens[$lastToken]['line'] === 1
                && $tokens[$lastToken]['content'] !== "\n"
            ) {
                return;
            }
        }

        $error    = 'End of line character is invalid; expected "%s" but found "%s"';
        $expected = $this->eolChar;
        $expected = str_replace("\n", '\n', $expected);
        $expected = str_replace("\r", '\r', $expected);
        $data     = array(
                     $expected,
                     $found,
                    );

        $fix = $phpcsFile->addFixableError($error, $stackPtr, 'InvalidEOLChar', $data);

        if ($fix === true) {
            $tokens = $phpcsFile->getTokens();
            switch ($this->eolChar) {
            case '\n':
                $eolChar = "\n";
                break;
            case '\r':
                $eolChar = "\r";
                break;
            case '\r\n':
                $eolChar = "\r\n";
                break;
            default:
                $eolChar = $this->eolChar;
                break;
            }

            for ($i = $stackPtr; $i < $phpcsFile->numTokens; $i++) {
                if (isset($tokens[($i + 1)]) === false
                    || $tokens[($i + 1)]['line'] > $tokens[$i]['line']
                ) {
                    // Token is the last on a line.
                    $newContent  = rtrim($tokens[$i]['content'], "\r\n");
                    $newContent .= $eolChar;
                    $phpcsFile->fixer->replaceToken($i, $newContent);
                }
            }
        }//end if

        // Ignore the rest of the file.
        return ($phpcsFile->numTokens + 1);

    }//end process()


}//end class
