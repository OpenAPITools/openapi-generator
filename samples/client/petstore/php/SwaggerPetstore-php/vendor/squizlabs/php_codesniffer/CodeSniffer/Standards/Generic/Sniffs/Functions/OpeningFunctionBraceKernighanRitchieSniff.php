<?php
/**
 * Generic_Sniffs_Functions_OpeningFunctionBraceKernighanRitchieSniff.
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
 * Generic_Sniffs_Functions_OpeningFunctionBraceKernighanRitchieSniff.
 *
 * Checks that the opening brace of a function is on the same line
 * as the function declaration.
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
class Generic_Sniffs_Functions_OpeningFunctionBraceKernighanRitchieSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Should this sniff check function braces?
     *
     * @var bool
     */
    public $checkFunctions = true;

    /**
     * Should this sniff check closure braces?
     *
     * @var bool
     */
    public $checkClosures = false;


    /**
     * Registers the tokens that this sniff wants to listen for.
     *
     * @return void
     */
    public function register()
    {
        return array(
                T_FUNCTION,
                T_CLOSURE,
               );

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

        if (isset($tokens[$stackPtr]['scope_opener']) === false) {
            return;
        }

        if (($tokens[$stackPtr]['code'] === T_FUNCTION
            && (bool) $this->checkFunctions === false)
            || ($tokens[$stackPtr]['code'] === T_CLOSURE
            && (bool) $this->checkClosures === false)
        ) {
            return;
        }

        $openingBrace = $tokens[$stackPtr]['scope_opener'];
        $functionLine = $tokens[$tokens[$stackPtr]['parenthesis_closer']]['line'];
        $braceLine    = $tokens[$openingBrace]['line'];

        $lineDifference = ($braceLine - $functionLine);

        if ($lineDifference > 0) {
            $phpcsFile->recordMetric($stackPtr, 'Function opening brace placement', 'new line');
            $error = 'Opening brace should be on the same line as the declaration';
            $fix   = $phpcsFile->addFixableError($error, $openingBrace, 'BraceOnNewLine');
            if ($fix === true) {
                $closeBracket = $tokens[$stackPtr]['parenthesis_closer'];
                $phpcsFile->fixer->beginChangeset();
                $phpcsFile->fixer->addContent($closeBracket, ' {');
                $phpcsFile->fixer->replaceToken($openingBrace, '');
                $phpcsFile->fixer->endChangeset();
            }
        }

        $phpcsFile->recordMetric($stackPtr, 'Function opening brace placement', 'same line');

        $next = $phpcsFile->findNext(T_WHITESPACE, ($openingBrace + 1), null, true);
        if ($tokens[$next]['line'] === $tokens[$openingBrace]['line']) {
            if ($next === $tokens[$stackPtr]['scope_closer']) {
                // Ignore empty functions.
                return;
            }

            $error = 'Opening brace must be the last content on the line';
            $fix   = $phpcsFile->addFixableError($error, $openingBrace, 'ContentAfterBrace');
            if ($fix === true) {
                $phpcsFile->fixer->addNewline($openingBrace);
            }
        }

        // Only continue checking if the opening brace looks good.
        if ($lineDifference > 0) {
            return;
        }

        $closeBracket = $tokens[$stackPtr]['parenthesis_closer'];
        if ($tokens[$stackPtr]['code'] === T_CLOSURE) {
            $use = $phpcsFile->findNext(T_USE, ($closeBracket + 1), $tokens[$stackPtr]['scope_opener']);
            if ($use !== false) {
                $openBracket  = $phpcsFile->findNext(T_OPEN_PARENTHESIS, ($use + 1));
                $closeBracket = $tokens[$openBracket]['parenthesis_closer'];
            }
        }

        if ($tokens[($closeBracket + 1)]['code'] !== T_WHITESPACE) {
            $length = 0;
        } else if ($tokens[($closeBracket + 1)]['content'] === "\t") {
            $length = '\t';
        } else {
            $length = strlen($tokens[($closeBracket + 1)]['content']);
        }

        if ($length !== 1) {
            $error = 'Expected 1 space after closing parenthesis; found %s';
            $data  = array($length);
            $fix   = $phpcsFile->addFixableError($error, $closeBracket, 'SpaceAfterBracket', $data);
            if ($fix === true) {
                if ($length === 0 || $length === '\t') {
                    $phpcsFile->fixer->addContent($closeBracket, ' ');
                } else {
                    $phpcsFile->fixer->replaceToken(($closeBracket + 1), ' ');
                }
            }
        }

    }//end process()


}//end class
