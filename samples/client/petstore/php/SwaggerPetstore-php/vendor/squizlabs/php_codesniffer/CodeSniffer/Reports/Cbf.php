<?php
/**
 * CBF report for PHP_CodeSniffer.
 *
 * This report implements the various auto-fixing features of the
 * PHPCBF script and is not intended (or allowed) to be selected as a
 * report from the command line.
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
 * CBF report for PHP_CodeSniffer.
 *
 * This report implements the various auto-fixing features of the
 * PHPCBF script and is not intended (or allowed) to be selected as a
 * report from the command line.
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
class PHP_CodeSniffer_Reports_Cbf implements PHP_CodeSniffer_Report
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
        $cliValues = $phpcsFile->phpcs->cli->getCommandLineValues();
        $errors    = $phpcsFile->getFixableCount();
        if ($errors !== 0) {
            if (empty($cliValues['files']) === false) {
                ob_end_clean();
                $errors    = $phpcsFile->getFixableCount();
                $startTime = microtime(true);
                echo "\t=> Fixing file: $errors/$errors violations remaining";
            }

            $fixed = $phpcsFile->fixer->fixFile();
        }

        if (empty($cliValues['files']) === true) {
            // Replacing STDIN, so output current file to STDOUT
            // even if nothing was fixed. Exit here because we
            // can't process any more than 1 file in this setup.
            echo $phpcsFile->fixer->getContents();
            ob_end_flush();
            exit(1);
        }

        if ($errors === 0) {
            return false;
        }

        if ($fixed === false) {
            echo 'ERROR';
        } else {
            echo 'DONE';
        }

        $timeTaken = ((microtime(true) - $startTime) * 1000);
        if ($timeTaken < 1000) {
            $timeTaken = round($timeTaken);
            echo " in {$timeTaken}ms".PHP_EOL;
        } else {
            $timeTaken = round(($timeTaken / 1000), 2);
            echo " in $timeTaken secs".PHP_EOL;
        }

        if ($fixed === true) {
            $newFilename = $report['filename'].$cliValues['phpcbf-suffix'];
            $newContent  = $phpcsFile->fixer->getContents();
            file_put_contents($newFilename, $newContent);

            if ($newFilename === $report['filename']) {
                echo "\t=> File was overwritten".PHP_EOL;
            } else {
                echo "\t=> Fixed file written to ".basename($newFilename).PHP_EOL;
            }
        }

        ob_start();

        return $fixed;

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
        echo "Fixed $totalFiles files".PHP_EOL;

    }//end generate()


}//end class
