<?php
/**
 * Squiz_Sniffs_PHP_DisallowMultipleAssignmentsSniff.
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
 * Squiz_Sniffs_PHP_DisallowMultipleAssignmentsSniff.
 *
 * Ensures that there is only one value assignment on a line, and that it is
 * the first thing on the line.
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
class Squiz_Sniffs_PHP_DisallowMultipleAssignmentsSniff implements PHP_CodeSniffer_Sniff
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
     * @param int                  $stackPtr  The position of the current token in the
     *                                        stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        // Ignore default value assignments in function definitions.
        $function = $phpcsFile->findPrevious(array(T_FUNCTION, T_CLOSURE), ($stackPtr - 1), null, false, null, true);
        if ($function !== false) {
            $opener = $tokens[$function]['parenthesis_opener'];
            $closer = $tokens[$function]['parenthesis_closer'];
            if ($opener < $stackPtr && $closer > $stackPtr) {
                return;
            }
        }

        /*
            The general rule is:
            Find an equal sign and go backwards along the line. If you hit an
            end bracket, skip to the opening bracket. When you find a variable,
            stop. That variable must be the first non-empty token on the line
            or in the statement. If not, throw an error.
        */

        for ($varToken = ($stackPtr - 1); $varToken >= 0; $varToken--) {
            // Skip brackets.
            if (isset($tokens[$varToken]['parenthesis_opener']) === true && $tokens[$varToken]['parenthesis_opener'] < $varToken) {
                $varToken = $tokens[$varToken]['parenthesis_opener'];
                continue;
            }

            if (isset($tokens[$varToken]['bracket_opener']) === true) {
                $varToken = $tokens[$varToken]['bracket_opener'];
                continue;
            }

            if ($tokens[$varToken]['code'] === T_SEMICOLON) {
                // We've reached the next statement, so we
                // didn't find a variable.
                return;
            }

            if ($tokens[$varToken]['code'] === T_VARIABLE) {
                // We found our variable.
                break;
            }
        }//end for

        if ($varToken <= 0) {
            // Didn't find a variable.
            return;
        }

        // Deal with this type of variable: self::$var by setting the var
        // token to be "self" rather than "$var".
        if ($tokens[($varToken - 1)]['code'] === T_DOUBLE_COLON) {
            $varToken = ($varToken - 2);
        }

        // Deal with this type of variable: $obj->$var by setting the var
        // token to be "$obj" rather than "$var".
        if ($tokens[($varToken - 1)]['code'] === T_OBJECT_OPERATOR) {
            $varToken = ($varToken - 2);
        }

        // Deal with this type of variable: $$var by setting the var
        // token to be "$" rather than "$var".
        if ($tokens[($varToken - 1)]['content'] === '$') {
            $varToken--;
        }

        // Ignore member var definitions.
        $prev = $phpcsFile->findPrevious(T_WHITESPACE, ($varToken - 1), null, true);
        if (isset(PHP_CodeSniffer_Tokens::$scopeModifiers[$tokens[$prev]['code']]) === true) {
            return;
        }

        if ($tokens[$prev]['code'] === T_STATIC) {
            return;
        }

        // Make sure this variable is the first thing in the statement.
        $varLine  = $tokens[$varToken]['line'];
        $prevLine = 0;
        for ($i = ($varToken - 1); $i >= 0; $i--) {
            if ($tokens[$i]['code'] === T_SEMICOLON) {
                // We reached the end of the statement.
                return;
            }

            if ($tokens[$i]['code'] === T_INLINE_THEN) {
                // We reached the end of the inline THEN statement.
                return;
            }

            if ($tokens[$i]['code'] === T_INLINE_ELSE) {
                // We reached the end of the inline ELSE statement.
                return;
            }

            if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$tokens[$i]['code']]) === false) {
                $prevLine = $tokens[$i]['line'];
                break;
            }
        }//end for

        // Ignore the first part of FOR loops as we are allowed to
        // assign variables there even though the variable is not the
        // first thing on the line. Also ignore WHILE loops.
        if ($tokens[$i]['code'] === T_OPEN_PARENTHESIS && isset($tokens[$i]['parenthesis_owner']) === true) {
            $owner = $tokens[$i]['parenthesis_owner'];
            if ($tokens[$owner]['code'] === T_FOR || $tokens[$owner]['code'] === T_WHILE) {
                return;
            }
        }

        if ($prevLine === $varLine) {
            $error = 'Assignments must be the first block of code on a line';
            $phpcsFile->addError($error, $stackPtr, 'Found');
        }

    }//end process()


}//end class
