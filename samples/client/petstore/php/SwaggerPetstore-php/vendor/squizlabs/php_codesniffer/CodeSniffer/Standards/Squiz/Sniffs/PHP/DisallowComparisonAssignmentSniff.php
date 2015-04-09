<?php
/**
 * Squiz_Sniffs_PHP_DisallowComparisonAssignmentSniff.
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
 * Squiz_Sniffs_PHP_DisallowComparisonAssignmentSniff.
 *
 * Ensures that the value of a comparison is not assigned to a variable.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Squiz_Sniffs_PHP_DisallowComparisonAssignmentSniff implements PHP_CodeSniffer_Sniff
{


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

        // Ignore default value assignments in function definitions.
        $function = $phpcsFile->findPrevious(T_FUNCTION, ($stackPtr - 1), null, false, null, true);
        if ($function !== false) {
            $opener = $tokens[$function]['parenthesis_opener'];
            $closer = $tokens[$function]['parenthesis_closer'];
            if ($opener < $stackPtr && $closer > $stackPtr) {
                return;
            }
        }

        // Ignore values in array definitions.
        $array = $phpcsFile->findNext(
            T_ARRAY,
            ($stackPtr + 1),
            null,
            false,
            null,
            true
        );

        if ($array !== false) {
            return;
        }

        // Ignore function calls.
        $ignore = array(
                   T_STRING,
                   T_WHITESPACE,
                   T_OBJECT_OPERATOR,
                  );

        $next = $phpcsFile->findNext($ignore, ($stackPtr + 1), null, true);
        if ($tokens[$next]['code'] === T_OPEN_PARENTHESIS
            && $tokens[($next - 1)]['code'] === T_STRING
        ) {
            // Code will look like: $var = myFunction(
            // and will be ignored.
            return;
        }

        $endStatement = $phpcsFile->findNext(T_SEMICOLON, ($stackPtr + 1));
        if ($tokens[$stackPtr]['conditions'] !== $tokens[$endStatement]['conditions']) {
            // This statement doesn't end with a semicolon, which is the case for
            // the last expression in a for loop.
            return;
        }

        for ($i = ($stackPtr + 1); $i < $endStatement; $i++) {
            if (isset(PHP_CodeSniffer_Tokens::$comparisonTokens[$tokens[$i]['code']]) === true) {
                $error = 'The value of a comparison must not be assigned to a variable';
                $phpcsFile->addError($error, $stackPtr, 'AssignedComparison');
                break;
            }

            if (isset(PHP_CodeSniffer_Tokens::$booleanOperators[$tokens[$i]['code']]) === true
                || $tokens[$i]['code'] === T_BOOLEAN_NOT
            ) {
                $error = 'The value of a boolean operation must not be assigned to a variable';
                $phpcsFile->addError($error, $stackPtr, 'AssignedBool');
                break;
            }
        }

    }//end process()


}//end class
