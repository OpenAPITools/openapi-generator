<?php
/**
 * Summary report for PHP_CodeSniffer.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Gabriele Santini <gsantini@sqli.com>
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2009-2014 SQLI <www.sqli.com>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Summary report for PHP_CodeSniffer.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Gabriele Santini <gsantini@sqli.com>
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2009-2014 SQLI <www.sqli.com>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PHP_CodeSniffer_Reports_Summary implements PHP_CodeSniffer_Report
{

    /**
     * TRUE if this report needs error messages instead of just totals.
     *
     * @var boolean
     */
    public $recordErrors = false;

    /**
     * An array of process files and their error data.
     *
     * @var boolean
     */
    private $_reportFiles = array();


    /**
     * Generate a partial report for a single processed file.
     *
     * Function should return TRUE if it printed or stored data about the file
     * and FALSE if it ignored the file. Returning TRUE indicates that the file and
     * its data should be counted in the grand totals.
     *
     * @param array                $report      Prepared report data.
     * @param PHP_CodeSniffer_File $phpcsFile   The file being reported on.
     * @param boolean              $showSources Show sources?
     * @param int                  $width       Maximum allowed line width.
     *
     * @return boolean
     */
    public function generateFileReport(
        $report,
        PHP_CodeSniffer_File $phpcsFile,
        $showSources=false,
        $width=80
    ) {
        if (PHP_CODESNIFFER_VERBOSITY === 0
            && $report['errors'] === 0
            && $report['warnings'] === 0
        ) {
            // Nothing to print.
            return false;
        }

        $this->_reportFiles[$report['filename']] = array(
                                                    'errors'   => $report['errors'],
                                                    'warnings' => $report['warnings'],
                                                    'strlen'   => strlen($report['filename']),
                                                   );

        return true;

    }//end generateFileReport()


    /**
     * Generates a summary of errors and warnings for each file processed.
     *
     * @param string  $cachedData    Any partial report data that was returned from
     *                               generateFileReport during the run.
     * @param int     $totalFiles    Total number of files processed during the run.
     * @param int     $totalErrors   Total number of errors found during the run.
     * @param int     $totalWarnings Total number of warnings found during the run.
     * @param int     $totalFixable  Total number of problems that can be fixed.
     * @param boolean $showSources   Show sources?
     * @param int     $width         Maximum allowed line width.
     * @param boolean $toScreen      Is the report being printed to screen?
     *
     * @return void
     */
    public function generate(
        $cachedData,
        $totalFiles,
        $totalErrors,
        $totalWarnings,
        $totalFixable,
        $showSources=false,
        $width=80,
        $toScreen=true
    ) {

        if (empty($this->_reportFiles) === true) {
            return;
        }

        // Make sure the report width isn't too big.
        $maxLength = 0;
        foreach ($this->_reportFiles as $file => $data) {
            $maxLength = max($maxLength, $data['strlen']);
        }

        $width = min($width, ($maxLength + 21));
        $width = max($width, 70);

        echo PHP_EOL."\033[1m".'PHP CODE SNIFFER REPORT SUMMARY'."\033[0m".PHP_EOL;
        echo str_repeat('-', $width).PHP_EOL;
        echo "\033[1m".'FILE'.str_repeat(' ', ($width - 20)).'ERRORS  WARNINGS'."\033[0m".PHP_EOL;
        echo str_repeat('-', $width).PHP_EOL;

        foreach ($this->_reportFiles as $file => $data) {
            $padding = ($width - 18 - $data['strlen']);
            if ($padding < 0) {
                $file    = '...'.substr($file, (($padding * -1) + 3));
                $padding = 0;
            }

            echo $file.str_repeat(' ', $padding).'  ';
            if ($data['errors'] !== 0) {
                echo "\033[31m".$data['errors']."\033[0m";
                echo str_repeat(' ', (8 - strlen((string) $data['errors'])));
            } else {
                echo '0       ';
            }

            if ($data['warnings'] !== 0) {
                echo "\033[33m".$data['warnings']."\033[0m";
            } else {
                echo '0';
            }

            echo PHP_EOL;
        }//end foreach

        echo str_repeat('-', $width).PHP_EOL;
        echo "\033[1mA TOTAL OF $totalErrors ERROR";
        if ($totalErrors !== 1) {
            echo 'S';
        }

        echo ' AND '.$totalWarnings.' WARNING';
        if ($totalWarnings !== 1) {
            echo 'S';
        }

        echo ' WERE FOUND IN '.$totalFiles.' FILE';
        if ($totalFiles !== 1) {
            echo 'S';
        }

        echo "\033[0m";

        if ($totalFixable > 0) {
            echo PHP_EOL.str_repeat('-', $width).PHP_EOL;
            echo "\033[1mPHPCBF CAN FIX $totalFixable OF THESE SNIFF VIOLATIONS AUTOMATICALLY\033[0m";
        }

        echo PHP_EOL.str_repeat('-', $width).PHP_EOL.PHP_EOL;

        if ($toScreen === true && PHP_CODESNIFFER_INTERACTIVE === false) {
            PHP_CodeSniffer_Reporting::printRunTime();
        }

    }//end generate()


}//end class
