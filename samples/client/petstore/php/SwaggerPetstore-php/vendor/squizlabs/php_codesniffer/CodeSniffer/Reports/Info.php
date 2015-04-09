<?php
/**
 * Info report for PHP_CodeSniffer.
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
 * Info report for PHP_CodeSniffer.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PHP_CodeSniffer_Reports_Info implements PHP_CodeSniffer_Report
{

    /**
     * TRUE if this report needs error messages instead of just totals.
     *
     * @var boolean
     */
    public $recordErrors = false;

    /**
     * A cache of metrics collected during the run.
     *
     * @var array
     */
    private $_metricCache = array();


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
        $metrics = $phpcsFile->getMetrics();
        foreach ($metrics as $metric => $data) {
            if (isset($this->_metricCache[$metric]) === false) {
                $this->_metricCache[$metric] = array();
            }

            foreach ($data['values'] as $value => $locations) {
                $locations = array_unique($locations);
                $count     = count($locations);

                if (isset($this->_metricCache[$metric][$value]) === false) {
                    $this->_metricCache[$metric][$value] = $count;
                } else {
                    $this->_metricCache[$metric][$value] += $count;
                }
            }
        }//end foreach

        return true;

    }//end generateFileReport()


    /**
     * Prints the source of all errors and warnings.
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
        if (empty($this->_metricCache) === true) {
            // Nothing to show.
            return;
        }

        ksort($this->_metricCache);

        echo PHP_EOL."\033[1m".'PHP CODE SNIFFER INFORMATION REPORT'."\033[0m".PHP_EOL;
        echo str_repeat('-', 70).PHP_EOL;

        foreach ($this->_metricCache as $metric => $values) {
            $winner      = '';
            $winnerCount = 0;
            $totalCount  = 0;
            foreach ($values as $value => $count) {
                $totalCount += $count;
                if ($count > $winnerCount) {
                    $winner      = $value;
                    $winnerCount = $count;
                }
            }

            $winPercent = round(($winnerCount / $totalCount * 100), 2);
            echo "$metric: \033[4m$winner\033[0m [$winnerCount/$totalCount, $winPercent%]".PHP_EOL;

            asort($values);
            $values = array_reverse($values, true);
            foreach ($values as $value => $count) {
                if ($value === $winner) {
                    continue;
                }

                $percent = round(($count / $totalCount * 100), 2);
                echo "\t$value => $count ($percent%)".PHP_EOL;
            }

            echo PHP_EOL;
        }//end foreach

        echo str_repeat('-', 70).PHP_EOL;

        if ($toScreen === true && PHP_CODESNIFFER_INTERACTIVE === false) {
            PHP_CodeSniffer_Reporting::printRunTime();
        }

    }//end generate()


}//end class
