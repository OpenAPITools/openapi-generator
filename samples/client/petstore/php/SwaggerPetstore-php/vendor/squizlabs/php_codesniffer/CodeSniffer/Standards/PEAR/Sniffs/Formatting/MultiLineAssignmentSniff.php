<?php
/**
 * PEAR_Sniffs_Functions_FunctionDeclarationSniff.
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
 * PEAR_Sniffs_Formatting_MultiLineAssignmentSniff.
 *
 * If an assignment goes over two lines, ensure the equal sign is indented.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PEAR_Sniffs_Formatting_MultiLineAssignmentSniff implements PHP_CodeSniffer_Sniff
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
        return array(T_EQUAL);

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

        // Equal sign can't be the last thing on the line.
        $next = $phpcsFile->findNext(T_WHITESPACE, ($stackPtr + 1), null, true);
        if ($next === false) {
            // Bad assignment.
            return;
        }

        if ($tokens[$next]['line'] !== $tokens[$stackPtr]['line']) {
            $error = 'Multi-line assignments must have the equal sign on the second line';
            $phpcsFile->addError($error, $stackPtr, 'EqualSignLine');
            return;
        }

        // Make sure it is the first thing on the line, otherwise we ignore it.
        $prev = $phpcsFile->findPrevious(T_WHITESPACE, ($stackPtr - 1), false, true);
        if ($prev === false) {
            // Bad assignment.
            return;
        }

        if ($tokens[$prev]['line'] === $tokens[$stackPtr]['line']) {
            return;
        }

        // Find the required indent based on the ident of the previous line.
        $assignmentIndent = 0;
        $prevLine         = $tokens[$prev]['line'];
        for ($i = ($prev - 1); $i >= 0; $i--) {
            if ($tokens[$i]['line'] !== $prevLine) {
                $i++;
                break;
            }
        }

        if ($tokens[$i]['code'] === T_WHITESPACE) {
            $assignmentIndent = strlen($tokens[$i]['content']);
        }

        // Find the actual indent.
        $prev = $phpcsFile->findPrevious(T_WHITESPACE, ($stackPtr - 1));

        $expectedIndent = ($assignmentIndent + $this->indent);
        $foundIndent    = strlen($tokens[$prev]['content']);
        if ($foundIndent !== $expectedIndent) {
            $error = 'Multi-line assignment not indented correctly; expected %s spaces but found %s';
            $data  = array(
                      $expectedIndent,
                      $foundIndent,
                     );
            $phpcsFile->addError($error, $stackPtr, 'Indent', $data);
        }

    }//end process()


}//end class
