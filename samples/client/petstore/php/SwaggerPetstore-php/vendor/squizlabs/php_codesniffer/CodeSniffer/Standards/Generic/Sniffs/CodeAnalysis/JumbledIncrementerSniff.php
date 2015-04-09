<?php
/**
 * This file is part of the CodeAnalysis add-on for PHP_CodeSniffer.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Manuel Pichler <mapi@manuel-pichler.de>
 * @copyright 2007-2014 Manuel Pichler. All rights reserved.
 * @license   http://www.opensource.org/licenses/bsd-license.php BSD License
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Detects incrementer jumbling in for loops.
 *
 * This rule is based on the PMD rule catalog. The jumbling incrementer sniff
 * detects the usage of one and the same incrementer into an outer and an inner
 * loop. Even it is intended this is confusing code.
 *
 * <code>
 * class Foo
 * {
 *     public function bar($x)
 *     {
 *         for ($i = 0; $i < 10; $i++)
 *         {
 *             for ($k = 0; $k < 20; $i++)
 *             {
 *                 echo 'Hello';
 *             }
 *         }
 *     }
 * }
 * </code>
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Manuel Pichler <mapi@manuel-pichler.de>
 * @copyright 2007-2014 Manuel Pichler. All rights reserved.
 * @license   http://www.opensource.org/licenses/bsd-license.php BSD License
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Generic_Sniffs_CodeAnalysis_JumbledIncrementerSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Registers the tokens that this sniff wants to listen for.
     *
     * @return int[]
     */
    public function register()
    {
        return array(T_FOR);

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
        $token  = $tokens[$stackPtr];

        // Skip for-loop without body.
        if (isset($token['scope_opener']) === false) {
            return;
        }

        // Find incrementors for outer loop.
        $outer = $this->findIncrementers($tokens, $token);

        // Skip if empty.
        if (count($outer) === 0) {
            return;
        }

        // Find nested for loops.
        $start = ++$token['scope_opener'];
        $end   = --$token['scope_closer'];

        for (; $start <= $end; ++$start) {
            if ($tokens[$start]['code'] !== T_FOR) {
                continue;
            }

            $inner = $this->findIncrementers($tokens, $tokens[$start]);
            $diff  = array_intersect($outer, $inner);

            if (count($diff) !== 0) {
                $error = 'Loop incrementor (%s) jumbling with inner loop';
                $data  = array(join(', ', $diff));
                $phpcsFile->addWarning($error, $stackPtr, 'Found', $data);
            }
        }

    }//end process()


    /**
     * Get all used variables in the incrementer part of a for statement.
     *
     * @param array(integer=>array) $tokens Array with all code sniffer tokens.
     * @param array(string=>mixed)  $token  Current for loop token
     *
     * @return string[] List of all found incrementer variables.
     */
    protected function findIncrementers(array $tokens, array $token)
    {
        // Skip invalid statement.
        if (isset($token['parenthesis_opener']) === false) {
            return array();
        }

        $start = ++$token['parenthesis_opener'];
        $end   = --$token['parenthesis_closer'];

        $incrementers = array();
        $semicolons   = 0;
        for ($next = $start; $next <= $end; ++$next) {
            $code = $tokens[$next]['code'];
            if ($code === T_SEMICOLON) {
                ++$semicolons;
            } else if ($semicolons === 2 && $code === T_VARIABLE) {
                $incrementers[] = $tokens[$next]['content'];
            }
        }

        return $incrementers;

    }//end findIncrementers()


}//end class
