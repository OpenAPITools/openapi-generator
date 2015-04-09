<?php
/**
 * Full report for PHP_CodeSniffer.
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
 * Full report for PHP_CodeSniffer.
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
class PHP_CodeSniffer_Reports_Full implements PHP_CodeSniffer_Report
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
        if ($report['errors'] === 0 && $report['warnings'] === 0) {
            // Nothing to print.
            return false;
        }

        // The length of the word ERROR or WARNING; used for padding.
        if ($report['warnings'] > 0) {
            $typeLength = 7;
        } else {
            $typeLength = 5;
        }

        // Work out the max line number length for formatting.
        $maxLineNumLength = max(array_map('strlen', array_keys($report['messages'])));

        // The padding that all lines will require that are
        // printing an error message overflow.
        $paddingLine2  = str_repeat(' ', ($maxLineNumLength + 1));
        $paddingLine2 .= ' | ';
        $paddingLine2 .= str_repeat(' ', $typeLength);
        $paddingLine2 .= ' | ';
        if ($report['fixable'] > 0) {
            $paddingLine2 .= '    ';
        }

        $paddingLength = strlen($paddingLine2);

        // Make sure the report width isn't too big.
        $maxErrorLength = 0;
        foreach ($report['messages'] as $line => $lineErrors) {
            foreach ($lineErrors as $column => $colErrors) {
                foreach ($colErrors as $error) {
                    $length = strlen($error['message']);
                    if ($showSources === true) {
                        $length += (strlen($error['source']) + 3);
                    }

                    $maxErrorLength = max($maxErrorLength, ($length + 1));
                }
            }
        }

        $file       = $report['filename'];
        $fileLength = strlen($file);
        $maxWidth   = max(($fileLength + 6), ($maxErrorLength + $paddingLength));
        $width      = min($width, $maxWidth);
        if ($width < 70) {
            $width = 70;
        }

        echo PHP_EOL."\033[1mFILE: ";
        if ($fileLength <= ($width - 6)) {
            echo $file;
        } else {
            echo '...'.substr($file, ($fileLength - ($width - 6)));
        }

        echo "\033[0m".PHP_EOL;
        echo str_repeat('-', $width).PHP_EOL;

        echo "\033[1m".'FOUND '.$report['errors'].' ERROR';
        if ($report['errors'] !== 1) {
            echo 'S';
        }

        if ($report['warnings'] > 0) {
            echo ' AND '.$report['warnings'].' WARNING';
            if ($report['warnings'] !== 1) {
                echo 'S';
            }
        }

        echo ' AFFECTING '.count($report['messages']).' LINE';
        if (count($report['messages']) !== 1) {
            echo 'S';
        }

        echo "\033[0m".PHP_EOL;
        echo str_repeat('-', $width).PHP_EOL;

        // The maximum amount of space an error message can use.
        $maxErrorSpace = ($width - $paddingLength - 1);
        if ($showSources === true) {
            // Account for the chars used to print colors.
            $maxErrorSpace += 8;
        }

        foreach ($report['messages'] as $line => $lineErrors) {
            foreach ($lineErrors as $column => $colErrors) {
                foreach ($colErrors as $error) {
                    $message = $error['message'];
                    if ($showSources === true) {
                        $message = "\033[1m".$message."\033[0m".' ('.$error['source'].')';
                    }

                    // The padding that goes on the front of the line.
                    $padding  = ($maxLineNumLength - strlen($line));
                    $errorMsg = wordwrap(
                        $message,
                        $maxErrorSpace,
                        PHP_EOL.$paddingLine2
                    );

                    echo ' '.str_repeat(' ', $padding).$line.' | ';
                    if ($error['type'] === 'ERROR') {
                        echo "\033[31mERROR\033[0m";
                        if ($report['warnings'] > 0) {
                            echo '  ';
                        }
                    } else {
                        echo "\033[33mWARNING\033[0m";
                    }

                    echo ' | ';
                    if ($report['fixable'] > 0) {
                        echo '[';
                        if ($error['fixable'] === true) {
                            echo 'x';
                        } else {
                            echo ' ';
                        }

                        echo '] ';
                    }

                    echo $errorMsg.PHP_EOL;
                }//end foreach
            }//end foreach
        }//end foreach

        echo str_repeat('-', $width).PHP_EOL;
        if ($report['fixable'] > 0) {
            echo "\033[1m".'PHPCBF CAN FIX THE '.$report['fixable'].' MARKED SNIFF VIOLATIONS AUTOMATICALLY'."\033[0m".PHP_EOL;
            echo str_repeat('-', $width).PHP_EOL;
        }

        echo PHP_EOL;
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
        if ($cachedData === '') {
            return;
        }

        echo $cachedData;

        if ($toScreen === true && PHP_CODESNIFFER_INTERACTIVE === false) {
            PHP_CodeSniffer_Reporting::printRunTime();
        }

    }//end generate()


}//end class
