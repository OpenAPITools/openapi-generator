<?php
/**
 * Phing task for composer validation.
 *
 * @copyright 2012 Clay Loveless <clay@php.net>
 * @license   http://claylo.mit-license.org/2012/ MIT License
 */

require_once 'phing/Task.php';

class ComposerLintTask extends Task
{
    protected $dir = null;
    protected $file = null;
    protected $passthru = false;
    protected $composer = null;

    /**
     * The setter for the dir
     *
     * @param string $str Directory to crawl recursively for composer files
     */
    public function setDir($str)
    {
        $this->dir = $str;
    }

    /**
     * The setter for the file
     *
     * @param string $str Individual file to validate
     */
    public function setFile($str)
    {
        $this->file = $str;
    }

    /**
     * Whether to use PHP's passthru() function instead of exec()
     *
     * @param boolean $passthru If passthru shall be used
     */
    public function setPassthru($passthru)
    {
        $this->passthru = (bool) $passthru;
    }

    /**
     * Composer to execute. If unset, will attempt composer.phar in project
     * basedir, and if that fails, will attempt global composer
     * installation.
     *
     * @param string $str Individual file to validate
     */
    public function setComposer($str)
    {
        $this->file = $str;
    }

    /**
     * The init method: do init steps
     */
    public function init()
    {
        // nothing needed here
    }

    /**
     * The main entry point
     */
    public function main()
    {
        if ($this->composer === null) {
            $this->findComposer();
        }

        $files = array();
        if (!empty($this->file) && file_exists($this->file)) {
            $files[] = $this->file;
        }

        if (!empty($this->dir)) {
            $found = $this->findFiles();
            foreach ($found as $file) {
                $files[] = $this->dir . DIRECTORY_SEPARATOR . $file;
            }
        }

        foreach ($files as $file) {

            $cmd = $this->composer . ' validate ' . $file;
            $cmd = escapeshellcmd($cmd);

            if ($this->passthru) {
                $retval = null;
                passthru($cmd, $retval);
                if ($retval == 1) {
                    throw new BuildException('invalid composer.json');
                }
            } else {
                $out = array();
                $retval = null;
                exec($cmd, $out, $retval);
                if ($retval == 1) {
                    $err = join("\n", $out);
                    throw new BuildException($err);
                } else {
                    $this->log($out[0]);
                }
            }

        }

    }

    /**
     * Find the composer.json files using Phing's directory scanner
     *
     * @return array
     */
    protected function findFiles()
    {
        $ds = new DirectoryScanner();
        $ds->setBasedir($this->dir);
        $ds->setIncludes(array('**/composer.json'));
        $ds->scan();
        return $ds->getIncludedFiles();
    }

    /**
     * Find composer installation
     *
     */
    protected function findComposer()
    {
        $basedir = $this->project->getBasedir();
        $php = $this->project->getProperty('php.interpreter');

        if (file_exists($basedir . '/composer.phar')) {
            $this->composer = "$php $basedir/composer.phar";
        } else {
            $out = array();
            exec('which composer', $out);
            if (empty($out)) {
                throw new BuildException(
                    'Could not determine composer location.'
                );
            }
            $this->composer = $out[0];
        }
    }
}
