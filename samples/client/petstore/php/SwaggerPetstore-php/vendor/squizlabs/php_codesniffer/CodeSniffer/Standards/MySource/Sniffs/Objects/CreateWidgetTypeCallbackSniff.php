<?php
/**
 * Ensures the create() method of widget types properly uses callbacks.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer_MySource
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Ensures the create() method of widget types properly uses callbacks.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer_MySource
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class MySource_Sniffs_Objects_CreateWidgetTypeCallbackSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of tokenizers this sniff supports.
     *
     * @var array
     */
    public $supportedTokenizers = array('JS');


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_OBJECT);

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

        $className = $phpcsFile->findPrevious(T_STRING, ($stackPtr - 1));
        if (substr(strtolower($tokens[$className]['content']), -10) !== 'widgettype') {
            return;
        }

        // Search for a create method.
        $create = $phpcsFile->findNext(T_PROPERTY, $stackPtr, $tokens[$stackPtr]['bracket_closer'], null, 'create');
        if ($create === false) {
            return;
        }

        $function = $phpcsFile->findNext(array(T_WHITESPACE, T_COLON), ($create + 1), null, true);
        if ($tokens[$function]['code'] !== T_FUNCTION
            && $tokens[$function]['code'] !== T_CLOSURE
        ) {
            return;
        }

        $start = ($tokens[$function]['scope_opener'] + 1);
        $end   = ($tokens[$function]['scope_closer'] - 1);

        // Check that the first argument is called "callback".
        $arg = $phpcsFile->findNext(T_WHITESPACE, ($tokens[$function]['parenthesis_opener'] + 1), null, true);
        if ($tokens[$arg]['content'] !== 'callback') {
            $error = 'The first argument of the create() method of a widget type must be called "callback"';
            $phpcsFile->addError($error, $arg, 'FirstArgNotCallback');
        }

        /*
            Look for return statements within the function. They cannot return
            anything and must be preceded by the callback.call() line. The
            callback itself must contain "self" or "this" as the first argument
            and there needs to be a call to the callback function somewhere
            in the create method. All calls to the callback function must be
            followed by a return statement or the end of the method.
        */

        $foundCallback  = false;
        $passedCallback = false;
        $nestedFunction = null;
        for ($i = $start; $i <= $end; $i++) {
            // Keep track of nested functions.
            if ($nestedFunction !== null) {
                if ($i === $nestedFunction) {
                    $nestedFunction = null;
                    continue;
                }
            } else if (($tokens[$i]['code'] === T_FUNCTION
                || $tokens[$i]['code'] === T_CLOSURE)
                && isset($tokens[$i]['scope_closer']) === true
            ) {
                $nestedFunction = $tokens[$i]['scope_closer'];
                continue;
            }

            if ($nestedFunction === null && $tokens[$i]['code'] === T_RETURN) {
                // Make sure return statements are not returning anything.
                if ($tokens[($i + 1)]['code'] !== T_SEMICOLON) {
                    $error = 'The create() method of a widget type must not return a value';
                    $phpcsFile->addError($error, $i, 'ReturnValue');
                }

                continue;
            } else if ($tokens[$i]['code'] !== T_STRING
                || $tokens[$i]['content'] !== 'callback'
            ) {
                continue;
            }

            // If this is the form "callback.call(" then it is a call
            // to the callback function.
            if ($tokens[($i + 1)]['code'] !== T_OBJECT_OPERATOR
                || $tokens[($i + 2)]['content'] !== 'call'
                || $tokens[($i + 3)]['code'] !== T_OPEN_PARENTHESIS
            ) {
                // One last chance; this might be the callback function
                // being passed to another function, like this
                // "this.init(something, callback, something)".
                if (isset($tokens[$i]['nested_parenthesis']) === false) {
                    continue;
                }

                // Just make sure those brackets dont belong to anyone,
                // like an IF or FOR statement.
                foreach ($tokens[$i]['nested_parenthesis'] as $bracket) {
                    if (isset($tokens[$bracket]['parenthesis_owner']) === true) {
                        continue(2);
                    }
                }

                // Note that we use this endBracket down further when checking
                // for a RETURN statement.
                $endBracket = end($tokens[$i]['nested_parenthesis']);
                $bracket    = key($tokens[$i]['nested_parenthesis']);

                $prev = $phpcsFile->findPrevious(
                    PHP_CodeSniffer_Tokens::$emptyTokens,
                    ($bracket - 1),
                    null,
                    true
                );

                if ($tokens[$prev]['code'] !== T_STRING) {
                    // This is not a function passing the callback.
                    continue;
                }

                $passedCallback = true;
            }//end if

            $foundCallback = true;

            if ($passedCallback === false) {
                // The first argument must be "this" or "self".
                $arg = $phpcsFile->findNext(T_WHITESPACE, ($i + 4), null, true);
                if ($tokens[$arg]['content'] !== 'this'
                    && $tokens[$arg]['content'] !== 'self'
                ) {
                    $error = 'The first argument passed to the callback function must be "this" or "self"';
                    $phpcsFile->addError($error, $arg, 'FirstArgNotSelf');
                }
            }

            // Now it must be followed by a return statement or the end of the function.
            if ($passedCallback === false) {
                $endBracket = $tokens[($i + 3)]['parenthesis_closer'];
            }

            for ($next = $endBracket; $next <= $end; $next++) {
                // Skip whitespace so we find the next content after the call.
                if (isset(PHP_CodeSniffer_Tokens::$emptyTokens[$tokens[$next]['code']]) === true) {
                    continue;
                }

                // Skip closing braces like END IF because it is not executable code.
                if ($tokens[$next]['code'] === T_CLOSE_CURLY_BRACKET) {
                    continue;
                }

                // We don't care about anything on the current line, like a
                // semicolon. It doesn't matter if there are other statements on the
                // line because another sniff will check for those.
                if ($tokens[$next]['line'] === $tokens[$endBracket]['line']) {
                    continue;
                }

                break;
            }

            if ($next !== $tokens[$function]['scope_closer']
                && $tokens[$next]['code'] !== T_RETURN
            ) {
                $error = 'The call to the callback function must be followed by a return statement if it is not the last statement in the create() method';
                $phpcsFile->addError($error, $i, 'NoReturn');
            }
        }//end for

        if ($foundCallback === false) {
            $error = 'The create() method of a widget type must call the callback function';
            $phpcsFile->addError($error, $create, 'CallbackNotCalled');
        }

    }//end process()


}//end class
