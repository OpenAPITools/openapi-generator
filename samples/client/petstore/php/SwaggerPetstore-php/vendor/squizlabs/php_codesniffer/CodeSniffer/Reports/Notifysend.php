<?php
/**
 * Notify-send report for PHP_CodeSniffer.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Christian Weiske <christian.weiske@netresearch.de>
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2012-2014 Christian Weiske
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

/**
 * Notify-send report for PHP_CodeSniffer.
 *
 * Supported configuration parameters:
 * - notifysend_path    - Full path to notify-send cli command
 * - notifysend_timeout - Timeout in milliseconds
 * - notifysend_showok  - Show "ok, all fine" messages (0/1)
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Christian Weiske <christian.weiske@netresearch.de>
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2012-2014 Christian Weiske
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @version   Release: @package_version@
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */
class PHP_CodeSniffer_Reports_Notifysend implements PHP_CodeSniffer_Report
{

    /**
     * Notification timeout in milliseconds.
     *
     * @var integer
     */
    protected $timeout = 3000;

    /**
     * Path to notify-send command.
     *
     * @var string
     */
    protected $path = 'notify-send';

    /**
     * Show "ok, all fine" messages.
     *
     * @var boolean
     */
    protected $showOk = true;

    /**
     * Version of installed notify-send executable.
     *
     * @var string
     */
    protected $version = null;

    /**
     * A record of the last file checked.
     *
     * This is used in case we only checked one file and need to print
     * the name/path of the file. We wont have access to the checked file list
     * after the run has been completed.
     *
     * @var string
     */
    private $_lastCheckedFile = '';


    /**
     * Load configuration data.
     */
    public function __construct()
    {
        $path = PHP_CodeSniffer::getConfigData('notifysend_path');
        if ($path !== null) {
            $this->path = $path;
        }

        $timeout = PHP_CodeSniffer::getConfigData('notifysend_timeout');
        if ($timeout !== null) {
            $this->timeout = (int) $timeout;
        }

        $showOk = PHP_CodeSniffer::getConfigData('notifysend_showok');
        if ($showOk !== null) {
            $this->showOk = (boolean) $showOk;
        }

        $this->version = str_replace(
            'notify-send ',
            '',
            exec($this->path.' --version')
        );

    }//end __construct()


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
        // We don't need to print anything, but we want this file counted
        // in the total number of checked files even if it has no errors.
        $this->_lastCheckedFile = $report['filename'];
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
        $msg = $this->generateMessage($totalFiles, $totalErrors, $totalWarnings);
        if ($msg === null) {
            if ($this->showOk === true) {
                $this->notifyAllFine();
            }
        } else {
            $this->notifyErrors($msg);
        }

    }//end generate()


    /**
     * Generate the error message to show to the user.
     *
     * @param int $totalFiles    Total number of files processed during the run.
     * @param int $totalErrors   Total number of errors found during the run.
     * @param int $totalWarnings Total number of warnings found during the run.
     *
     * @return string Error message or NULL if no error/warning found.
     */
    protected function generateMessage($totalFiles, $totalErrors, $totalWarnings)
    {
        if ($totalErrors === 0 && $totalWarnings === 0) {
            // Nothing to print.
            return null;
        }

        $msg = '';
        if ($totalFiles > 1) {
            $msg .= 'Checked '.$totalFiles.' files'.PHP_EOL;
        } else {
            $msg .= $this->_lastCheckedFile.PHP_EOL;
        }

        if ($totalWarnings > 0) {
            $msg .= $totalWarnings.' warnings'.PHP_EOL;
        }

        if ($totalErrors > 0) {
            $msg .= $totalErrors.' errors'.PHP_EOL;
        }

        return $msg;

    }//end generateMessage()


    /**
     * Tell the user that all is fine and no error/warning has been found.
     *
     * @return void
     */
    protected function notifyAllFine()
    {
        $cmd  = $this->getBasicCommand();
        $cmd .= ' -i info';
        $cmd .= ' "PHP CodeSniffer: Ok"';
        $cmd .= ' "All fine"';
        exec($cmd);

    }//end notifyAllFine()


    /**
     * Tell the user that errors/warnings have been found.
     *
     * @param string $msg Message to display.
     *
     * @return void
     */
    protected function notifyErrors($msg)
    {
        $cmd  = $this->getBasicCommand();
        $cmd .= ' -i error';
        $cmd .= ' "PHP CodeSniffer: Error"';
        $cmd .= ' '.escapeshellarg(trim($msg));
        exec($cmd);

    }//end notifyErrors()


    /**
     * Generate and return the basic notify-send command string to execute.
     *
     * @return string Shell command with common parameters.
     */
    protected function getBasicCommand()
    {
        $cmd  = escapeshellcmd($this->path);
        $cmd .= ' --category dev.validate';
        $cmd .= ' -h int:transient:1';
        $cmd .= ' -t '.(int) $this->timeout;
        if (version_compare($this->version, '0.7.3', '>=') === true) {
            $cmd .= ' -a phpcs';
        }

        return $cmd;

    }//end getBasicCommand()


}//end class
