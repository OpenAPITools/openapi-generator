<?php
/**
 * Ensures that systems, asset types and libs are included before they are used.
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

if (class_exists('PHP_CodeSniffer_Standards_AbstractScopeSniff', true) === false) {
    $error = 'Class PHP_CodeSniffer_Standards_AbstractScopeSniff not found';
    throw new PHP_CodeSniffer_Exception($error);
}

/**
 * Ensures that systems, asset types and libs are included before they are used.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer_MySource
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class MySource_Sniffs_Channels_IncludeSystemSniff extends PHP_CodeSniffer_Standards_AbstractScopeSniff
{

    /**
     * A list of classes that don't need to be included.
     *
     * @var array(string)
     */
    private $_ignore = array(
                        'self'                      => true,
                        'static'                    => true,
                        'parent'                    => true,
                        'channels'                  => true,
                        'basesystem'                => true,
                        'dal'                       => true,
                        'init'                      => true,
                        'pdo'                       => true,
                        'util'                      => true,
                        'ziparchive'                => true,
                        'phpunit_framework_assert'  => true,
                        'abstractmysourceunittest'  => true,
                        'abstractdatacleanunittest' => true,
                        'exception'                 => true,
                        'abstractwidgetwidgettype'  => true,
                        'domdocument'               => true,
                       );


    /**
     * Constructs a Squiz_Sniffs_Scope_MethodScopeSniff.
     */
    public function __construct()
    {
        parent::__construct(array(T_FUNCTION), array(T_DOUBLE_COLON, T_EXTENDS), true);

    }//end __construct()


    /**
     * Processes the function tokens within the class.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file where this token was found.
     * @param integer              $stackPtr  The position where the token was found.
     * @param integer              $currScope The current scope opener token.
     *
     * @return void
     */
    protected function processTokenWithinScope(
        PHP_CodeSniffer_File $phpcsFile,
        $stackPtr,
        $currScope
    ) {
        $tokens = $phpcsFile->getTokens();

        // Determine the name of the class that the static function
        // is being called on.
        $classNameToken = $phpcsFile->findPrevious(
            T_WHITESPACE,
            ($stackPtr - 1),
            null,
            true
        );

        // Don't process class names represented by variables as this can be
        // an inexact science.
        if ($tokens[$classNameToken]['code'] === T_VARIABLE) {
            return;
        }

        $className = $tokens[$classNameToken]['content'];
        if (isset($this->_ignore[strtolower($className)]) === true) {
            return;
        }

        $includedClasses = array();

        $fileName = strtolower($phpcsFile->getFilename());
        $matches  = array();
        if (preg_match('|/systems/(.*)/([^/]+)?actions.inc$|', $fileName, $matches) !== 0) {
            // This is an actions file, which means we don't
            // have to include the system in which it exists.
            $includedClasses[$matches[2]] = true;

            // Or a system it implements.
            $class      = $phpcsFile->getCondition($stackPtr, T_CLASS);
            $implements = $phpcsFile->findNext(T_IMPLEMENTS, $class, ($class + 10));
            if ($implements !== false) {
                $implementsClass     = $phpcsFile->findNext(T_STRING, $implements);
                $implementsClassName = strtolower($tokens[$implementsClass]['content']);
                if (substr($implementsClassName, -7) === 'actions') {
                    $includedClasses[substr($implementsClassName, 0, -7)] = true;
                }
            }
        }

        // Go searching for includeSystem and includeAsset calls within this
        // function, or the inclusion of .inc files, which
        // would be library files.
        for ($i = ($currScope + 1); $i < $stackPtr; $i++) {
            $name = $this->getIncludedClassFromToken($phpcsFile, $tokens, $i);
            if ($name !== false) {
                $includedClasses[$name] = true;
                // Special case for Widgets cause they are, well, special.
            } else if (strtolower($tokens[$i]['content']) === 'includewidget') {
                $typeName = $phpcsFile->findNext(T_CONSTANT_ENCAPSED_STRING, ($i + 1));
                $typeName = trim($tokens[$typeName]['content'], " '");
                $includedClasses[strtolower($typeName).'widgettype'] = true;
            }
        }

        // Now go searching for includeSystem, includeAsset or require/include
        // calls outside our scope. If we are in a class, look outside the
        // class. If we are not, look outside the function.
        $condPtr = $currScope;
        if ($phpcsFile->hasCondition($stackPtr, T_CLASS) === true) {
            foreach ($tokens[$stackPtr]['conditions'] as $condPtr => $condType) {
                if ($condType === T_CLASS) {
                    break;
                }
            }
        }

        for ($i = 0; $i < $condPtr; $i++) {
            // Skip other scopes.
            if (isset($tokens[$i]['scope_closer']) === true) {
                $i = $tokens[$i]['scope_closer'];
                continue;
            }

            $name = $this->getIncludedClassFromToken($phpcsFile, $tokens, $i);
            if ($name !== false) {
                $includedClasses[$name] = true;
            }
        }

        // If we are in a testing class, we might have also included
        // some systems and classes in our setUp() method.
        $setupFunction = null;
        if ($phpcsFile->hasCondition($stackPtr, T_CLASS) === true) {
            foreach ($tokens[$stackPtr]['conditions'] as $condPtr => $condType) {
                if ($condType === T_CLASS) {
                    // Is this is a testing class?
                    $name = $phpcsFile->findNext(T_STRING, $condPtr);
                    $name = $tokens[$name]['content'];
                    if (substr($name, -8) === 'UnitTest') {
                        // Look for a method called setUp().
                        $end      = $tokens[$condPtr]['scope_closer'];
                        $function = $phpcsFile->findNext(T_FUNCTION, ($condPtr + 1), $end);
                        while ($function !== false) {
                            $name = $phpcsFile->findNext(T_STRING, $function);
                            if ($tokens[$name]['content'] === 'setUp') {
                                $setupFunction = $function;
                                break;
                            }

                            $function = $phpcsFile->findNext(T_FUNCTION, ($function + 1), $end);
                        }
                    }
                }
            }//end foreach
        }//end if

        if ($setupFunction !== null) {
            $start = ($tokens[$setupFunction]['scope_opener'] + 1);
            $end   = $tokens[$setupFunction]['scope_closer'];
            for ($i = $start; $i < $end; $i++) {
                $name = $this->getIncludedClassFromToken($phpcsFile, $tokens, $i);
                if ($name !== false) {
                    $includedClasses[$name] = true;
                }
            }
        }//end if

        if (isset($includedClasses[strtolower($className)]) === false) {
            $error = 'Static method called on non-included class or system "%s"; include system with Channels::includeSystem() or include class with require_once';
            $data  = array($className);
            $phpcsFile->addError($error, $stackPtr, 'NotIncludedCall', $data);
        }

    }//end processTokenWithinScope()


    /**
     * Processes a token within the scope that this test is listening to.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file where the token was found.
     * @param int                  $stackPtr  The position in the stack where
     *                                        this token was found.
     *
     * @return void
     */
    protected function processTokenOutsideScope(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $tokens = $phpcsFile->getTokens();

        if ($tokens[$stackPtr]['code'] === T_EXTENDS) {
            // Find the class name.
            $classNameToken = $phpcsFile->findNext(T_STRING, ($stackPtr + 1));
            $className      = $tokens[$classNameToken]['content'];
        } else {
            // Determine the name of the class that the static function
            // is being called on. But don't process class names represented by
            // variables as this can be an inexact science.
            $classNameToken = $phpcsFile->findPrevious(T_WHITESPACE, ($stackPtr - 1), null, true);
            if ($tokens[$classNameToken]['code'] === T_VARIABLE) {
                return;
            }

            $className = $tokens[$classNameToken]['content'];
        }

        // Some systems are always available.
        if (isset($this->_ignore[strtolower($className)]) === true) {
            return;
        }

        $includedClasses = array();

        $fileName = strtolower($phpcsFile->getFilename());
        $matches  = array();
        if (preg_match('|/systems/([^/]+)/([^/]+)?actions.inc$|', $fileName, $matches) !== 0) {
            // This is an actions file, which means we don't
            // have to include the system in which it exists
            // We know the system from the path.
            $includedClasses[$matches[1]] = true;
        }

        // Go searching for includeSystem, includeAsset or require/include
        // calls outside our scope.
        for ($i = 0; $i < $stackPtr; $i++) {
            // Skip classes and functions as will we never get
            // into their scopes when including this file, although
            // we have a chance of getting into IF's, WHILE's etc.
            if (($tokens[$i]['code'] === T_CLASS
                || $tokens[$i]['code'] === T_INTERFACE
                || $tokens[$i]['code'] === T_FUNCTION)
                && isset($tokens[$i]['scope_closer']) === true
            ) {
                $i = $tokens[$i]['scope_closer'];
                continue;
            }

            $name = $this->getIncludedClassFromToken($phpcsFile, $tokens, $i);
            if ($name !== false) {
                $includedClasses[$name] = true;
                // Special case for Widgets cause they are, well, special.
            } else if (strtolower($tokens[$i]['content']) === 'includewidget') {
                $typeName = $phpcsFile->findNext(T_CONSTANT_ENCAPSED_STRING, ($i + 1));
                $typeName = trim($tokens[$typeName]['content'], " '");
                $includedClasses[strtolower($typeName).'widgettype'] = true;
            }
        }//end for

        if (isset($includedClasses[strtolower($className)]) === false) {
            if ($tokens[$stackPtr]['code'] === T_EXTENDS) {
                $error = 'Class extends non-included class or system "%s"; include system with Channels::includeSystem() or include class with require_once';
                $data  = array($className);
                $phpcsFile->addError($error, $stackPtr, 'NotIncludedExtends', $data);
            } else {
                $error = 'Static method called on non-included class or system "%s"; include system with Channels::includeSystem() or include class with require_once';
                $data  = array($className);
                $phpcsFile->addError($error, $stackPtr, 'NotIncludedCall', $data);
            }
        }

    }//end processTokenOutsideScope()


    /**
     * Determines the included class name from given token.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file where this token was found.
     * @param array                $tokens    The array of file tokens.
     * @param int                  $stackPtr  The position in the tokens array of the
     *                                        potentially included class.
     *
     * @return string
     */
    protected function getIncludedClassFromToken(
        PHP_CodeSniffer_File $phpcsFile,
        array $tokens,
        $stackPtr
    ) {
        if (strtolower($tokens[$stackPtr]['content']) === 'includesystem') {
            $systemName = $phpcsFile->findNext(T_CONSTANT_ENCAPSED_STRING, ($stackPtr + 1));
            $systemName = trim($tokens[$systemName]['content'], " '");
            return strtolower($systemName);
        } else if (strtolower($tokens[$stackPtr]['content']) === 'includeasset') {
            $typeName = $phpcsFile->findNext(T_CONSTANT_ENCAPSED_STRING, ($stackPtr + 1));
            $typeName = trim($tokens[$typeName]['content'], " '");
            return strtolower($typeName).'assettype';
        } else if (isset(PHP_CodeSniffer_Tokens::$includeTokens[$tokens[$stackPtr]['code']]) === true) {
            $filePath = $phpcsFile->findNext(T_CONSTANT_ENCAPSED_STRING, ($stackPtr + 1));
            $filePath = $tokens[$filePath]['content'];
            $filePath = trim($filePath, " '");
            $filePath = basename($filePath, '.inc');
            return strtolower($filePath);
        }

        return false;

    }//end getIncludedClassFromToken()


}//end class
