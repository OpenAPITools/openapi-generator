<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Config;

/**
 * Coveralls API configuration.
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class Configuration
{
    // same as ruby lib

    /**
     * repo_token.
     *
     * @var string
     */
    protected $repoToken;

    /**
     * service_name.
     *
     * @var string
     */
    protected $serviceName;

    // only for php lib

    /**
     * Absolute path to src directory to include coverage report.
     *
     * @var string
     */
    protected $srcDir;

    /**
     * Absolute paths to clover.xml.
     *
     * @var array
     */
    protected $cloverXmlPaths = array();

    /**
     * Absolute path to output json_file.
     *
     * @var string
     */
    protected $jsonPath;

    // from command option

    /**
     * Whether to send json_file to jobs API.
     *
     * @var boolean
     */
    protected $dryRun = true;

    /**
     * Whether to exclude source files that have no executable statements.
     *
     * @var boolean
     */
    protected $excludeNoStatements = false;

    /**
     * Whether to show log.
     *
     * @var boolean
     */
    protected $verbose = false;

    /**
     * Runtime environment name.
     *
     * @var string
     */
    protected $env = 'prod';

    // accessor

    /**
     * Set repository token.
     *
     * @param  string                                                 $repoToken
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    public function setRepoToken($repoToken)
    {
        $this->repoToken = $repoToken;

        return $this;
    }

    /**
     * Return whether repository token is configured.
     *
     * @return boolean
     */
    public function hasRepoToken()
    {
        return isset($this->repoToken);
    }

    /**
     * Return repository token.
     *
     * @return string|NULL
     */
    public function getRepoToken()
    {
        return $this->repoToken;
    }

    /**
     * Set service name.
     *
     * @param  string                                                 $serviceName
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    public function setServiceName($serviceName)
    {
        $this->serviceName = $serviceName;

        return $this;
    }

    /**
     * Return whether the service name is configured.
     *
     * @return boolean
     */
    public function hasServiceName()
    {
        return isset($this->serviceName);
    }

    /**
     * Return service name.
     *
     * @return string|NULL
     */
    public function getServiceName()
    {
        return $this->serviceName;
    }

    /**
     * Set absolute path to src directory to include coverage report.
     *
     * @param  string                                                 $srcDir
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    public function setSrcDir($srcDir)
    {
        $this->srcDir = $srcDir;

        return $this;
    }

    /**
     * Return absolute path to src directory to include coverage report.
     *
     * @return string
     */
    public function getSrcDir()
    {
        return $this->srcDir;
    }

    /**
     * Set absolute paths to clover.xml.
     *
     * @param  string                                                 $cloverXmlPaths
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    public function setCloverXmlPaths(array $cloverXmlPaths)
    {
        $this->cloverXmlPaths = $cloverXmlPaths;

        return $this;
    }

    /**
     * Add absolute path to clover.xml.
     *
     * @param  string                                                 $cloverXmlPath
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    public function addCloverXmlPath($cloverXmlPath)
    {
        $this->cloverXmlPaths[] = $cloverXmlPath;

        return $this;
    }

    /**
     * Return absolute path to clover.xml.
     *
     * @return string
     */
    public function getCloverXmlPaths()
    {
        return $this->cloverXmlPaths;
    }

    /**
     * Set absolute path to output json_file.
     *
     * @param  string                                                 $jsonPath
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    public function setJsonPath($jsonPath)
    {
        $this->jsonPath = $jsonPath;

        return $this;
    }

    /**
     * Return absolute path to output json_file.
     *
     * @return string
     */
    public function getJsonPath()
    {
        return $this->jsonPath;
    }

    /**
     * Set whether to send json_file to jobs API.
     *
     * @param  boolean                                                $dryRun
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    public function setDryRun($dryRun)
    {
        $this->dryRun = $dryRun;

        return $this;
    }

    /**
     * Return whether to send json_file to jobs API.
     *
     * @return boolean
     */
    public function isDryRun()
    {
        return $this->dryRun;
    }

    /**
     * Set whether to exclude source files that have no executable statements.
     *
     * @param  boolean                                                $excludeNoStatements
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    public function setExcludeNoStatements($excludeNoStatements)
    {
        $this->excludeNoStatements = $excludeNoStatements;

        return $this;
    }

    /**
     * Set whether to exclude source files that have no executable statements unless false.
     *
     * @param  boolean                                                $excludeNoStatements
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    public function setExcludeNoStatementsUnlessFalse($excludeNoStatements)
    {
        if ($excludeNoStatements) {
            $this->excludeNoStatements = true;
        }

        return $this;
    }

    /**
     * Return whether to exclude source files that have no executable statements.
     *
     * @return boolean
     */
    public function isExcludeNoStatements()
    {
        return $this->excludeNoStatements;
    }

    /**
     * Set whether to show log.
     *
     * @param  boolean                                                $verbose
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    public function setVerbose($verbose)
    {
        $this->verbose = $verbose;

        return $this;
    }

    /**
     * Return whether to show log.
     *
     * @return boolean
     */
    public function isVerbose()
    {
        return $this->verbose;
    }

    /**
     * Set runtime environment name.
     *
     * @param  string                                                 $env Runtime environment name.
     * @return \Contrib\Bundle\CoverallsV1Bundle\Config\Configuration
     */
    public function setEnv($env)
    {
        $this->env = $env;

        return $this;
    }

    /**
     * Return runtime environment name.
     *
     * @return string
     */
    public function getEnv()
    {
        return $this->env;
    }

    /**
     * Return whether the runtime environment is test.
     *
     * @return boolean
     */
    public function isTestEnv()
    {
        return $this->env === 'test';
    }

    /**
     * Return whether the runtime environment is dev.
     *
     * @return boolean
     */
    public function isDevEnv()
    {
        return $this->env === 'dev';
    }

    /**
     * Return whether the runtime environment is prod.
     *
     * @return boolean
     */
    public function isProdEnv()
    {
        return $this->env === 'prod';
    }
}
