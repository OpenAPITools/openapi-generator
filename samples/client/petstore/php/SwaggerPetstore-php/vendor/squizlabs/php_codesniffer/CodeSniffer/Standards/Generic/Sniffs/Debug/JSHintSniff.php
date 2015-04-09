<?php
/**
 * Generic_Sniffs_Debug_JSHintSniff.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Alexander Wei§ <aweisswa@gmx.de>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Generic_Sniffs_Debug_JSHintSniff.
 *
 * Runs jshint.js on the file.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @author    Alexander Wei§ <aweisswa@gmx.de>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Generic_Sniffs_Debug_JSHintSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of tokenizers this sniff supports.
     *
     * @var array
     */
    public $supportedTokenizers = array('JS');


    /**
     * Returns the token types that this sniff is interested in.
     *
     * @return int[]
     */
    public function register()
    {
        return array(T_OPEN_TAG);

    }//end register()


    /**
     * Processes the tokens that this sniff is interested in.
     *
     * @param PHP_CodeSniffer_File $phpcsFile The file where the token was found.
     * @param int                  $stackPtr  The position in the stack where
     *                                        the token was found.
     *
     * @return void
     * @throws PHP_CodeSniffer_Exception If jshint.js could not be run
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $fileName = $phpcsFile->getFilename();

        $rhinoPath  = PHP_CodeSniffer::getConfigData('rhino_path');
        $jshintPath = PHP_CodeSniffer::getConfigData('jshint_path');
        if ($rhinoPath === null || $jshintPath === null) {
            return;
        }

        $cmd = "$rhinoPath \"$jshintPath\" \"$fileName\"";
        $msg = exec($cmd, $output, $retval);

        if (is_array($output) === true) {
            foreach ($output as $finding) {
                $matches    = array();
                $numMatches = preg_match('/^(.+)\(.+:([0-9]+).*:[0-9]+\)$/', $finding, $matches);
                if ($numMatches === 0) {
                    continue;
                }

                $line    = (int) $matches[2];
                $message = 'jshint says: '.trim($matches[1]);
                $phpcsFile->addWarningOnLine($message, $line, 'ExternalTool');
            }
        }

        // Ignore the rest of the file.
        return ($phpcsFile->numTokens + 1);

    }//end process()


}//end class
