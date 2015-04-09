<?php
/**
 * JUnit report for PHP_CodeSniffer.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Oleg Lobach <oleg@lobach.info>
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * JUnit report for PHP_CodeSniffer.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Oleg Lobach <oleg@lobach.info>
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PHP_CodeSniffer_Reports_Junit implements PHP_CodeSniffer_Report
{

    /**
     * A count of tests that have been performed.
     *
     * @var int
     */
    private $_tests = 0;


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
        if (count($report['messages']) === 0) {
            $this->_tests++;
        } else {
            $this->_tests += ($report['errors'] + $report['warnings']);
        }

        $out = new XMLWriter;
        $out->openMemory();
        $out->setIndent(true);

        $out->startElement('testsuite');
        $out->writeAttribute('name', $report['filename']);

        if (count($report['messages']) === 0) {
            $out->writeAttribute('tests', 1);
            $out->writeAttribute('failures', 0);

            $out->startElement('testcase');
            $out->writeAttribute('name', $report['filename']);
            $out->endElement();
        } else {
            $failures = ($report['errors'] + $report['warnings']);
            $out->writeAttribute('tests', $failures);
            $out->writeAttribute('failures', $failures);

            foreach ($report['messages'] as $line => $lineErrors) {
                foreach ($lineErrors as $column => $colErrors) {
                    foreach ($colErrors as $error) {
                        $out->startElement('testcase');
                        $out->writeAttribute('name', $error['source'].' at '.$report['filename']." ($line:$column)");

                        $error['type'] = strtolower($error['type']);
                        if (PHP_CODESNIFFER_ENCODING !== 'utf-8') {
                            $error['message'] = iconv(PHP_CODESNIFFER_ENCODING, 'utf-8', $error['message']);
                        }

                        $out->startElement('failure');
                        $out->writeAttribute('type', $error['type']);
                        $out->writeAttribute('message', $error['message']);
                        $out->endElement();

                        $out->endElement();
                    }
                }
            }
        }//end if

        $out->endElement();
        echo $out->flush();
        return true;

    }//end generateFileReport()


    /**
     * Prints all violations for processed files, in a proprietary XML format.
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
        $failures = ($totalErrors + $totalWarnings);
        echo '<?xml version="1.0" encoding="UTF-8"?>'.PHP_EOL;
        echo '<testsuites name="PHP_CodeSniffer '.PHP_CodeSniffer::VERSION.'" tests="'.$this->_tests.'" failures="'.$failures.'">'.PHP_EOL;
        echo $cachedData;
        echo '</testsuites>'.PHP_EOL;

    }//end generate()


}//end class
