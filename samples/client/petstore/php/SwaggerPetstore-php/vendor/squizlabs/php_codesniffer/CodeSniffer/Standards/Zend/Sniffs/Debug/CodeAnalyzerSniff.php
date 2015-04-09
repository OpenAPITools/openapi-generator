<?php
/**
 * Zend_Sniffs_Debug_CodeAnalyzerSniff.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Holger Kral <holger.kral@zend.com>
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Zend_Sniffs_Debug_CodeAnalyzerSniff.
 *
 * Runs the Zend Code Analyzer (from Zend Studio) on the file.
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Holger Kral <holger.kral@zend.com>
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class Zend_Sniffs_Debug_CodeAnalyzerSniff implements PHP_CodeSniffer_Sniff
{


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
     * @return int
     */
    public function process(PHP_CodeSniffer_File $phpcsFile, $stackPtr)
    {
        $fileName     = $phpcsFile->getFilename();
        $analyzerPath = PHP_CodeSniffer::getConfigData('zend_ca_path');
        if (is_null($analyzerPath) === true) {
            return;
        }

        // In the command, 2>&1 is important because the code analyzer sends its
        // findings to stderr. $output normally contains only stdout, so using 2>&1
        // will pipe even stderr to stdout.
        $cmd = $analyzerPath.' '.$fileName.' 2>&1';

        // There is the possibility to pass "--ide" as an option to the analyzer.
        // This would result in an output format which would be easier to parse.
        // The problem here is that no cleartext error messages are returnwd; only
        // error-code-labels. So for a start we go for cleartext output.
        $exitCode = exec($cmd, $output, $retval);

        // Variable $exitCode is the last line of $output if no error occures, on
        // error it is numeric. Try to handle various error conditions and
        // provide useful error reporting.
        if (is_numeric($exitCode) === true && $exitCode > 0) {
            if (is_array($output) === true) {
                $msg = join('\n', $output);
            }

            throw new PHP_CodeSniffer_Exception("Failed invoking ZendCodeAnalyzer, exitcode was [$exitCode], retval was [$retval], output was [$msg]");
        }

        if (is_array($output) === true) {
            foreach ($output as $finding) {
                // The first two lines of analyzer output contain
                // something like this:
                // > Zend Code Analyzer 1.2.2
                // > Analyzing <filename>...
                // So skip these...
                $res = preg_match("/^.+\(line ([0-9]+)\):(.+)$/", $finding, $regs);
                if (empty($regs) === true || $res === false) {
                    continue;
                }

                $phpcsFile->addWarningOnLine(trim($regs[2]), $regs[1], 'ExternalTool');
            }
        }

        // Ignore the rest of the file.
        return ($phpcsFile->numTokens + 1);

    }//end process()


}//end class
