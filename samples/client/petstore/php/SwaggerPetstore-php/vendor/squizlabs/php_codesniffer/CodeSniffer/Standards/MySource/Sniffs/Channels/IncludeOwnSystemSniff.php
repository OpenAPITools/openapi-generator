<?php
/**
 * Ensures that a system does not include itself.
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
 * Ensures that a system does not include itself.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer_MySource
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class MySource_Sniffs_Channels_IncludeOwnSystemSniff implements PHP_CodeSniffer_Sniff
{


    /**
     * Returns an array of tokens this test wants to listen for.
     *
     * @return array
     */
    public function register()
    {
        return array(T_DOUBLE_COLON);

    }//end register()


    /**
     * Processes this sniff, when one of its tokens is encountered.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file being scanned.
     * @param int                  $stackPtr  The position of the current token in
     *                                        the stack passed in $tokens.
     *
     * @return void
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $fileName = $phpcsFile->getFilename();
        $matches  = array();
        if (preg_match('|/systems/(.*)/([^/]+)?actions.inc$|i', $fileName, $matches) === 0) {
            // Not an actions file.
            return;
        }

        $ownClass = $matches[2];
        $tokens   = $phpcsFile->getTokens();

        $typeName = $phpcsFile->findNext(T_CONSTANT_ENCAPSED_STRING, ($stackPtr + 2), null, false, true);
        $typeName = trim($tokens[$typeName]['content'], " '");
        switch (strtolower($tokens[($stackPtr + 1)]['content'])) {
        case 'includesystem' :
            $included = strtolower($typeName);
            break;
        case 'includeasset' :
            $included = strtolower($typeName).'assettype';
            break;
        case 'includewidget' :
            $included = strtolower($typeName).'widgettype';
            break;
        default:
            return;
        }

        if ($included === strtolower($ownClass)) {
            $error = "You do not need to include \"%s\" from within the system's own actions file";
            $data  = array($ownClass);
            $phpcsFile->addError($error, $stackPtr, 'NotRequired', $data);
        }

    }//end process()


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

        return false;

    }//end getIncludedClassFromToken()


}//end class
