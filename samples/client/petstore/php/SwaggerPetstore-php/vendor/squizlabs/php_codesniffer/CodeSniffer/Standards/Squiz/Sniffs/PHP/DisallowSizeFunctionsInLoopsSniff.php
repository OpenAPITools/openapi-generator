<?php
/**
 * Squiz_Sniffs_PHP_DisallowSizeFunctionsInLoopsSniff.
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
 * Squiz_Sniffs_PHP_DisallowSizeFunctionsInLoopsSniff.
 *
 * Bans the use of size-based functions in loop conditions.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Squiz_Sniffs_PHP_DisallowSizeFunctionsInLoopsSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of tokenizers this sniff supports.
     *
     * @var array
     */
    public $supportedTokenizers = array(
                                   'PHP',
                                   'JS',
                                  );

    /**
     * An array of functions we don't want in the condition of loops.
     *
     * @return array
     */
    protected $forbiddenFunctions = array(
                                     'PHP' => array(
                                               'sizeof' => true,
                                               'strlen' => true,
                                               'count'  => true,
                                              ),
                                     'JS'  => array('length' => true),
                                    );


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(
                T_WHILE,
                T_FOR,
               );

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
        $tokens       = $phpcsFile->getTokens();
        $tokenizer    = $phpcsFile->tokenizerType;
        $openBracket  = $tokens[$stackPtr]['parenthesis_opener'];
        $closeBracket = $tokens[$stackPtr]['parenthesis_closer'];

        if ($tokens[$stackPtr]['code'] === T_FOR) {
            // We only want to check the condition in FOR loops.
            $start = $phpcsFile->findNext(T_SEMICOLON, ($openBracket + 1));
            $end   = $phpcsFile->findPrevious(T_SEMICOLON, ($closeBracket - 1));
        } else {
            $start = $openBracket;
            $end   = $closeBracket;
        }

        for ($i = ($start + 1); $i < $end; $i++) {
            if ($tokens[$i]['code'] === T_STRING
                && isset($this->forbiddenFunctions[$tokenizer][$tokens[$i]['content']]) === true
            ) {
                $functionName = $tokens[$i]['content'];
                if ($tokenizer === 'JS') {
                    // Needs to be in the form object.function to be valid.
                    $prev = $phpcsFile->findPrevious(T_WHITESPACE, ($i - 1), null, true);
                    if ($prev === false || $tokens[$prev]['code'] !== T_OBJECT_OPERATOR) {
                        continue;
                    }

                    $functionName = 'object.'.$functionName;
                } else {
                    // Make sure it isn't a member var.
                    if ($tokens[($i - 1)]['code'] === T_OBJECT_OPERATOR) {
                        continue;
                    }

                    $functionName .= '()';
                }

                $error = 'The use of %s inside a loop condition is not allowed; assign the return value to a variable and use the variable in the loop condition instead';
                $data  = array($functionName);
                $phpcsFile->addError($error, $i, 'Found', $data);
            }//end if
        }//end for

    }//end process()


}//end class
