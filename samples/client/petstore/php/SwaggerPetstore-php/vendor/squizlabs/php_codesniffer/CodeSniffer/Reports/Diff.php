<?php
/**
 * Diff report for PHP_CodeSniffer.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2012 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Diff report for PHP_CodeSniffer.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2012 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PHP_CodeSniffer_Reports_Diff implements PHP_CodeSniffer_Report
{


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
        $errors = $phpcsFile->getFixableCount();
        if ($errors === 0) {
            return false;
        }

        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            ob_end_clean();
            echo "\t*** START FILE FIXING ***".PHP_EOL;
        }

        if (PHP_CODESNIFFER_CBF === true) {
            ob_end_clean();
            $startTime = microtime(true);
            echo "\t=> Fixing file: $errors/$errors violations remaining";
        }

        $fixed = $phpcsFile->fixer->fixFile();

        if (PHP_CODESNIFFER_CBF === true) {
            if ($fixed === false) {
                echo "\033[31mERROR\033[0m";
            } else {
                echo "\033[32mDONE\033[0m";
            }

            $timeTaken = ((microtime(true) - $startTime) * 1000);
            if ($timeTaken < 1000) {
                $timeTaken = round($timeTaken);
                echo " in {$timeTaken}ms".PHP_EOL;
            } else {
                $timeTaken = round(($timeTaken / 1000), 2);
                echo " in $timeTaken secs".PHP_EOL;
            }

            ob_start();
        }

        if (PHP_CODESNIFFER_VERBOSITY > 1) {
            echo "\t*** END FILE FIXING ***".PHP_EOL;
            ob_start();
        }

        if ($fixed === false) {
            return false;
        }

        if (PHP_CODESNIFFER_CBF === true) {
            // Diff without colours.
            $diff = $phpcsFile->fixer->generateDiff(null, false);
        } else {
            $diff = $phpcsFile->fixer->generateDiff();
        }

        if ($diff === '') {
            // Nothing to print.
            return false;
        }

        echo $diff;
        return true;

    }//end generateFileReport()


    /**
     * Prints all errors and warnings for each file processed.
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
        echo $cachedData;
        if ($toScreen === true) {
            echo PHP_EOL;
        }

    }//end generate()


}//end class
