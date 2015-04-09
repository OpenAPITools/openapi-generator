<?php
/**
 * Generic_Sniffs_Debug_ClosureLinterSniff.
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
 * Generic_Sniffs_Debug_ClosureLinterSniff.
 *
 * Runs gjslint on the file.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Generic_Sniffs_Debug_ClosureLinterSniff implements PHP_CodeSniffer_Sniff
{

    /**
     * A list of error codes that should show errors.
     *
     * All other error codes will show warnings.
     *
     * @var int
     */
    public $errorCodes = array();

    /**
     * A list of error codes to ignore.
     *
     * @var int
     */
    public $ignoreCodes = array();

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
     * @throws PHP_CodeSniffer_Exception If jslint.js could not be run
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $fileName = $phpcsFile->getFilename();

        $lintPath = PHP_CodeSniffer::getConfigData('gjslint_path');
        if ($lintPath === null) {
            return;
        }

        $cmd = "$lintPath --nosummary --notime --unix_mode \"$fileName\"";
        $msg = exec($cmd, $output, $retval);

        if (is_array($output) === false) {
            return;
        }

        foreach ($output as $finding) {
            $matches    = array();
            $numMatches = preg_match('/^(.*):([0-9]+):\(.*?([0-9]+)\)(.*)$/', $finding, $matches);
            if ($numMatches === 0) {
                continue;
            }

            // Skip error codes we are ignoring.
            $code = $matches[3];
            if (in_array($code, $this->ignoreCodes) === true) {
                continue;
            }

            $line  = (int) $matches[2];
            $error = trim($matches[4]);

            $message = 'gjslint says: (%s) %s';
            $data    = array(
                        $code,
                        $error,
                       );
            if (in_array($code, $this->errorCodes) === true) {
                $phpcsFile->addErrorOnLine($message, $line, 'ExternalToolError', $data);
            } else {
                $phpcsFile->addWarningOnLine($message, $line, 'ExternalTool', $data);
            }
        }//end foreach

        // Ignore the rest of the file.
        return ($phpcsFile->numTokens + 1);

    }//end process()


}//end class
