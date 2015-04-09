<?php
/**
 * An AbstractScopeTest allows for tests that extend from this class to
 * listen for tokens within a particular scope.
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
 * An AbstractScopeTest allows for tests that extend from this class to
 * listen for tokens within a particular scope.
 *
 * Below is a test that listens to methods that exist only within classes:
 * <code>
 * class ClassScopeTest extends PHP_CodeSniffer_Standards_AbstractScopeSniff
 * {
 *     public function __construct()
 *     {
 *         parent::__construct(array(T_CLASS), array(T_FUNCTION));
 *     }
 *
 *     protected function processTokenWithinScope(PHP_CodeSniffer_File $phpcsFile, $)
 *     {
 *         $className = $phpcsFile->getDeclarationName($currScope);
 *         echo 'encountered a method within class '.$className;
 *     }
 * }
 * </code>
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
abstract class PHP_CodeSniffer_Standards_AbstractScopeSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * The token types that this test wishes to listen to within the scope.
     *
     * @var array
     */
    private $_tokens = array();

    /**
     * The type of scope opener tokens that this test wishes to listen to.
     *
     * @var string
     */
    private $_scopeTokens = array();

    /**
     * True if this test should fire on tokens outside of the scope.
     *
     * @var boolean
     */
    private $_listenOutside = false;


    /**
     * Constructs a new AbstractScopeTest.
     *
     * @param array   $scopeTokens   The type of scope the test wishes to listen to.
     * @param array   $tokens        The tokens that the test wishes to listen to
     *                               within the scope.
     * @param boolean $listenOutside If true this test will also alert the
     *                               extending class when a token is found outside
     *                               the scope, by calling the
     *                               processTokenOutsideScope method.
     *
     * @see    PHP_CodeSniffer.getValidScopeTokeners()
     * @throws PHP_CodeSniffer_Exception If the specified tokens array is empty.
     */
    public function __construct(
        array $scopeTokens,
        array $tokens,
        $listenOutside=false
    ) {
        if (empty($scopeTokens) === true) {
            $error = 'The scope tokens list cannot be empty';
            throw new PHP_CodeSniffer_Exception($error);
        }

        if (empty($tokens) === true) {
            $error = 'The tokens list cannot be empty';
            throw new PHP_CodeSniffer_Exception($error);
        }

        $invalidScopeTokens = array_intersect($scopeTokens, $tokens);
        if (empty($invalidScopeTokens) === false) {
            $invalid = implode(', ', $invalidScopeTokens);
            $error   = "Scope tokens [$invalid] cant be in the tokens array";
            throw new PHP_CodeSniffer_Exception($error);
        }

        $this->_listenOutside = $listenOutside;
        $this->_scopeTokens   = array_flip($scopeTokens);
        $this->_tokens        = $tokens;

    }//end __construct()


    /**
     * The method that is called to register the tokens this test wishes to
     * listen to.
     *
     * DO NOT OVERRIDE THIS METHOD. Use the constructor of this class to register
     * for the desired tokens and scope.
     *
     * @return int[]
     * @see    __constructor()
     */
    public final function register()
    {
        return $this->_tokens;

    }//end register()


    /**
     * Processes the tokens that this test is listening for.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file where this token was found.
     * @param int                  $stackPtr  The position in the stack where this
     *                                        token was found.
     *
     * @return void
     * @see    processTokenWithinScope()
     */
    public final function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        $foundScope = false;
        foreach ($tokens[$stackPtr]['conditions'] as $scope => $code) {
            if (isset($this->_scopeTokens[$code]) === true) {
                $this->processTokenWithinScope($phpcsFile, $stackPtr, $scope);
                $foundScope = true;
            }
        }

        if ($this->_listenOutside === true && $foundScope === false) {
            $this->processTokenOutsideScope($phpcsFile, $stackPtr);
        }

    }//end process()


    /**
     * Processes a token that is found within the scope that this test is
     * listening to.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file where this token was found.
     * @param int                  $stackPtr  The position in the stack where this
     *                                        token was found.
     * @param int                  $currScope The position in the tokens array that
     *                                        opened the scope that this test is
     *                                        listening for.
     *
     * @return void
     */
    protected abstract function processTokenWithinScope(
        PHP_CodeSniffer_File $phpcsFile,
        $stackPtr,
        $currScope
    );


    /**
     * Processes a token that is found within the scope that this test is
     * listening to.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file where this token was found.
     * @param int                  $stackPtr  The position in the stack where this
     *                                        token was found.
     *
     * @return void
     */
    protected function processTokenOutsideScope(
        PHP_CodeSniffer_File $phpcsFile,
        $stackPtr
    ) {

    }//end processTokenOutsideScope()


}//end class
