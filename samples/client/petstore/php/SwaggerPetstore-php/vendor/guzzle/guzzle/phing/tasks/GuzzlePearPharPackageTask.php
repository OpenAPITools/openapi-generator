<?php
/**
 * This file is part of Guzzle's build process.
 *
 * @copyright 2012 Clay Loveless <clay@php.net>
 * @license   http://claylo.mit-license.org/2012/ MIT License
 */

require_once 'phing/Task.php';
require_once 'PEAR/PackageFileManager2.php';
require_once 'PEAR/PackageFileManager/File.php';
require_once 'PEAR/Packager.php';

class GuzzlePearPharPackageTask extends Task
{
    private $version;
    private $deploy = true;
    private $makephar = true;

    private $subpackages = array();

    public function setVersion($str)
    {
        $this->version = $str;
    }

    public function getVersion()
    {
        return $this->version;
    }

    public function setDeploy($deploy)
    {
        $this->deploy = (bool) $deploy;
    }

    public function getDeploy()
    {
        return $this->deploy;
    }

    public function setMakephar($makephar)
    {
        $this->makephar = (bool) $makephar;
    }

    public function getMakephar()
    {
        return $this->makephar;
    }

    private $basedir;
    private $guzzleinfo;
    private $changelog_release_date;
    private $changelog_notes = '-';

    public function main()
    {
        $this->basedir = $this->getProject()->getBasedir();

        if (!is_dir((string) $this->basedir.'/.subsplit')) {
            throw new BuildException('PEAR packaging requires .subsplit directory');
        }

        // main composer file
        $composer_file = file_get_contents((string) $this->basedir.'/.subsplit/composer.json');
        $this->guzzleinfo = json_decode($composer_file, true);

        // make sure we have a target
        $pearwork = (string) $this->basedir . '/build/pearwork';
        if (!is_dir($pearwork)) {
            mkdir($pearwork, 0777, true);
        }
        $pearlogs = (string) $this->basedir . '/build/artifacts/logs';
        if (!is_dir($pearlogs)) {
            mkdir($pearlogs, 0777, true);
        }

        $version = $this->getVersion();
        $this->grabChangelog();
        if ($version[0] == '2') {
            $this->log('building single PEAR package');
            $this->buildSinglePackage();
        } else {
            // $this->log("building PEAR subpackages");
            // $this->createSubPackages();
            // $this->log("building PEAR bundle package");
            $this->buildSinglePackage();
        }

        if ($this->getMakephar()) {
            $this->log("building PHAR");
            $this->getProject()->executeTarget('package-phar');
        }

        if ($this->getDeploy()) {
            $this->doDeployment();
        }
    }

    public function doDeployment()
    {
        $basedir = (string) $this->basedir;
        $this->log('beginning PEAR/PHAR deployment');

        chdir($basedir . '/build/pearwork');
        if (!is_dir('./channel')) {
            mkdir('./channel');
        }

        // Pull the PEAR channel down locally
        passthru('aws s3 sync s3://pear.guzzlephp.org ./channel');

        // add PEAR packages
        foreach (scandir('./') as $file) {
            if (substr($file, -4) == '.tgz') {
                passthru('pirum add ./channel ' . $file);
            }
        }

        // if we have a new phar, add it
        if ($this->getMakephar() && file_exists($basedir . '/build/artifacts/guzzle.phar')) {
            rename($basedir . '/build/artifacts/guzzle.phar', './channel/guzzle.phar');
        }

        // Sync up with the S3 bucket
        chdir($basedir . '/build/pearwork/channel');
        passthru('aws s3 sync . s3://pear.guzzlephp.org');
    }

    public function buildSinglePackage()
    {
        $v = $this->getVersion();
        $apiversion = $v[0] . '.0.0';

        $opts = array(
            'packagedirectory' => (string) $this->basedir . '/.subsplit/src/',
            'filelistgenerator' => 'file',
            'ignore' => array('*composer.json'),
            'baseinstalldir' => '/',
            'packagefile' => 'package.xml'
            //'outputdirectory' => (string) $this->basedir . '/build/pearwork/'
        );
        $pfm = new PEAR_PackageFileManager2();
        $pfm->setOptions($opts);
        $pfm->addRole('md', 'doc');
        $pfm->addRole('pem', 'php');
        $pfm->setPackage('Guzzle');
        $pfm->setSummary("Object-oriented PHP HTTP Client for PHP 5.3+");
        $pfm->setDescription($this->guzzleinfo['description']);
        $pfm->setPackageType('php');
        $pfm->setChannel('guzzlephp.org/pear');
        $pfm->setAPIVersion($apiversion);
        $pfm->setReleaseVersion($this->getVersion());
        $pfm->setAPIStability('stable');
        $pfm->setReleaseStability('stable');
        $pfm->setNotes($this->changelog_notes);
        $pfm->setPackageType('php');
        $pfm->setLicense('MIT', 'http://github.com/guzzle/guzzle/blob/master/LICENSE');
        $pfm->addMaintainer('lead', 'mtdowling', 'Michael Dowling', 'mtdowling@gmail.com', 'yes');
        $pfm->setDate($this->changelog_release_date);
        $pfm->generateContents();

        $phpdep = $this->guzzleinfo['require']['php'];
        $phpdep = str_replace('>=', '', $phpdep);
        $pfm->setPhpDep($phpdep);
        $pfm->addExtensionDep('required', 'curl');
        $pfm->setPearinstallerDep('1.4.6');
        $pfm->addPackageDepWithChannel('required', 'EventDispatcher', 'pear.symfony.com', '2.1.0');
        if (!empty($this->subpackages)) {
            foreach ($this->subpackages as $package) {
                $pkg = dirname($package);
                $pkg = str_replace('/', '_', $pkg);
                $pfm->addConflictingPackageDepWithChannel($pkg, 'guzzlephp.org/pear', false, $apiversion);
            }
        }

        ob_start();
        $startdir = getcwd();
        chdir((string) $this->basedir . '/build/pearwork');

        echo "DEBUGGING GENERATED PACKAGE FILE\n";
        $result = $pfm->debugPackageFile();
        if ($result) {
            $out = $pfm->writePackageFile();
            echo "\n\n\nWRITE PACKAGE FILE RESULT:\n";
            var_dump($out);
            // load up package file and build package
            $packager = new PEAR_Packager();
            echo "\n\n\nBUILDING PACKAGE FROM PACKAGE FILE:\n";
            $dest_package = $packager->package($opts['packagedirectory'].'package.xml');
            var_dump($dest_package);
        } else {
            echo "\n\n\nDEBUGGING RESULT:\n";
            var_dump($result);
        }
        echo "removing package.xml";
        unlink($opts['packagedirectory'].'package.xml');
        $log = ob_get_clean();
        file_put_contents((string) $this->basedir . '/build/artifacts/logs/pear_package.log', $log);
        chdir($startdir);
    }

    public function createSubPackages()
    {
        $this->findComponents();

        foreach ($this->subpackages as $package) {
            $baseinstalldir = dirname($package);
            $dir = (string) $this->basedir.'/.subsplit/src/' . $baseinstalldir;
            $composer_file = file_get_contents((string) $this->basedir.'/.subsplit/src/'. $package);
            $package_info = json_decode($composer_file, true);
            $this->log('building ' . $package_info['target-dir'] . ' subpackage');
            $this->buildSubPackage($dir, $baseinstalldir, $package_info);
        }
    }

    public function buildSubPackage($dir, $baseinstalldir, $info)
    {
        $package = str_replace('/', '_', $baseinstalldir);
        $opts = array(
            'packagedirectory' => $dir,
            'filelistgenerator' => 'file',
            'ignore' => array('*composer.json', '*package.xml'),
            'baseinstalldir' => '/' . $info['target-dir'],
            'packagefile' => 'package.xml'
        );
        $pfm = new PEAR_PackageFileManager2();
        $pfm->setOptions($opts);
        $pfm->setPackage($package);
        $pfm->setSummary($info['description']);
        $pfm->setDescription($info['description']);
        $pfm->setPackageType('php');
        $pfm->setChannel('guzzlephp.org/pear');
        $pfm->setAPIVersion('3.0.0');
        $pfm->setReleaseVersion($this->getVersion());
        $pfm->setAPIStability('stable');
        $pfm->setReleaseStability('stable');
        $pfm->setNotes($this->changelog_notes);
        $pfm->setPackageType('php');
        $pfm->setLicense('MIT', 'http://github.com/guzzle/guzzle/blob/master/LICENSE');
        $pfm->addMaintainer('lead', 'mtdowling', 'Michael Dowling', 'mtdowling@gmail.com', 'yes');
        $pfm->setDate($this->changelog_release_date);
        $pfm->generateContents();

        $phpdep = $this->guzzleinfo['require']['php'];
        $phpdep = str_replace('>=', '', $phpdep);
        $pfm->setPhpDep($phpdep);
        $pfm->setPearinstallerDep('1.4.6');

        foreach ($info['require'] as $type => $version) {
            if ($type == 'php') {
                continue;
            }
            if ($type == 'symfony/event-dispatcher') {
                $pfm->addPackageDepWithChannel('required', 'EventDispatcher', 'pear.symfony.com', '2.1.0');
            }
            if ($type == 'ext-curl') {
                $pfm->addExtensionDep('required', 'curl');
            }
            if (substr($type, 0, 6) == 'guzzle') {
                $gdep = str_replace('/', ' ', $type);
                $gdep = ucwords($gdep);
                $gdep = str_replace(' ', '_', $gdep);
                $pfm->addPackageDepWithChannel('required', $gdep, 'guzzlephp.org/pear', $this->getVersion());
            }
        }

        // can't have main Guzzle package AND sub-packages
        $pfm->addConflictingPackageDepWithChannel('Guzzle', 'guzzlephp.org/pear', false, $apiversion);

        ob_start();
        $startdir = getcwd();
        chdir((string) $this->basedir . '/build/pearwork');

        echo "DEBUGGING GENERATED PACKAGE FILE\n";
        $result = $pfm->debugPackageFile();
        if ($result) {
            $out = $pfm->writePackageFile();
            echo "\n\n\nWRITE PACKAGE FILE RESULT:\n";
            var_dump($out);
            // load up package file and build package
            $packager = new PEAR_Packager();
            echo "\n\n\nBUILDING PACKAGE FROM PACKAGE FILE:\n";
            $dest_package = $packager->package($opts['packagedirectory'].'/package.xml');
            var_dump($dest_package);
        } else {
            echo "\n\n\nDEBUGGING RESULT:\n";
            var_dump($result);
        }
        echo "removing package.xml";
        unlink($opts['packagedirectory'].'/package.xml');
        $log = ob_get_clean();
        file_put_contents((string) $this->basedir . '/build/artifacts/logs/pear_package_'.$package.'.log', $log);
        chdir($startdir);
    }

    public function findComponents()
    {
        $ds = new DirectoryScanner();
        $ds->setBasedir((string) $this->basedir.'/.subsplit/src');
        $ds->setIncludes(array('**/composer.json'));
        $ds->scan();
        $files = $ds->getIncludedFiles();
        $this->subpackages = $files;
    }

    public function grabChangelog()
    {
        $cl = file((string) $this->basedir.'/.subsplit/CHANGELOG.md');
        $notes = '';
        $in_version = false;
        $release_date = null;

        foreach ($cl as $line) {
            $line = trim($line);
            if (preg_match('/^\* '.$this->getVersion().' \(([0-9\-]+)\)$/', $line, $matches)) {
                $release_date = $matches[1];
                $in_version = true;
                continue;
            }
            if ($in_version && empty($line) && empty($notes)) {
                continue;
            }
            if ($in_version && ! empty($line)) {
                $notes .= $line."\n";
            }
            if ($in_version && empty($line) && !empty($notes)) {
                $in_version = false;
            }
        }
        $this->changelog_release_date = $release_date;

        if (! empty($notes)) {
            $this->changelog_notes = $notes;
        }
    }
}
