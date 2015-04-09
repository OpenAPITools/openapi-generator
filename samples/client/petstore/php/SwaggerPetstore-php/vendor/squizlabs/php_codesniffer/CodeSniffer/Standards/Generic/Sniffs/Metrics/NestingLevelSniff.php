<?php
/**
 * Checks the nesting level for methods.
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
 * Checks the nesting level for methods.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Johann-Peter Hartmann <hartmann@mayflower.de>
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2007-2014 Mayflower GmbH
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Generic_Sniffs_Metrics_NestingLevelSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A nesting level than this value will throw a warning.
     *
     * @var int
     */
    public $nestingLevel = 5;

    /**
     * A nesting level than this value will throw an error.
     *
     * @var int
     */
    public $absoluteNestingLevel = 10;


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

        // Ignore abstract methods.
        if (isset($tokens[$stackPtr]['scope_opener']) === false) {
            return;
        }

        // Detect start and end of this function definition.
        $start = $tokens[$stackPtr]['scope_opener'];
        $end   = $tokens[$stackPtr]['scope_closer'];

        $nestingLevel = 0;

        // Find the maximum nesting level of any token in the function.
        for ($i = ($start + 1); $i < $end; $i++) {
            $level = $tokens[$i]['level'];
            if ($nestingLevel < $level) {
                $nestingLevel = $level;
            }
        }

        // We subtract the nesting level of the function itself.
        $nestingLevel = ($nestingLevel - $tokens[$stackPtr]['level'] - 1);

        if ($nestingLevel > $this->absoluteNestingLevel) {
            $error = 'Function\'s nesting level (%s) exceeds allowed maximum of %s';
            $data  = array(
                      $nestingLevel,
                      $this->absoluteNestingLevel,
                     );
            $phpcsFile->addError($error, $stackPtr, 'MaxExceeded', $data);
        } else if ($nestingLevel > $this->nestingLevel) {
            $warning = 'Function\'s nesting level (%s) exceeds %s; consider refactoring the function';
            $data    = array(
                        $nestingLevel,
                        $this->nestingLevel,
                       );
            $phpcsFile->addWarning($warning, $stackPtr, 'TooHigh', $data);
        }

    }//end process()


}//end class
