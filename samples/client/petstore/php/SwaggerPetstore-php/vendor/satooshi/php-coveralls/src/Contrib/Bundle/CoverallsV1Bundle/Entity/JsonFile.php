<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Entity;

use Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Git;

/**
 * Data represents "json_file" of Coveralls API.
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class JsonFile extends Coveralls
{
    /**
     * Service name.
     *
     * @var string
     */
    protected $serviceName;

    /**
     * Service job id.
     *
     * @var string
     */
    protected $serviceJobId;

    /**
     * Service number (not documented).
     *
     * @var string
     */
    protected $serviceNumber;

    /**
     * Service event type (not documented).
     *
     * @var string
     */
    protected $serviceEventType;

    /**
     * Build URL of the project (not documented).
     *
     * @var string
     */
    protected $serviceBuildUrl;

    /**
     * Branch name (not documented).
     *
     * @var string
     */
    protected $serviceBranch;

    /**
     * Pull request info (not documented).
     *
     * @var string
     */
    protected $servicePullRequest;

    /**
     * Repository token.
     *
     * @var string
     */
    protected $repoToken;

    /**
     * Source files.
     *
     * @var \Contrib\Bundle\CoverallsV1Bundle\Entity\SourceFile[]
     */
    protected $sourceFiles = array();

    /**
     * Git data.
     *
     * @var array
     */
    protected $git;

    /**
     * A timestamp when the job ran. Must be parsable by Ruby.
     *
     * "2013-02-18 00:52:48 -0800"
     *
     * @var string
     */
    protected $runAt;

    /**
     * Metrics.
     *
     * @var Metrics
     */
    protected $metrics;

    // API

    /**
     * {@inheritdoc}
     *
     * @see \Contrib\Bundle\CoverallsBundle\Entity\ArrayConvertable::toArray()
     */
    public function toArray()
    {
        $array = array();

        $arrayMap = array(
            // json key => property name
            'service_name'         => 'serviceName',
            'service_job_id'       => 'serviceJobId',
            'service_number'       => 'serviceNumber',
            'service_build_url'    => 'serviceBuildUrl',
            'service_branch'       => 'serviceBranch',
            'service_pull_request' => 'servicePullRequest',
            'service_event_type'   => 'serviceEventType',
            'repo_token'           => 'repoToken',
            'git'                  => 'git',
            'run_at'               => 'runAt',
            'source_files'         => 'sourceFiles',
        );

        foreach ($arrayMap as $jsonKey => $propName) {
            if (isset($this->$propName)) {
                $array[$jsonKey] = $this->toJsonProperty($this->$propName);
            }
        }

        return $array;
    }

    /**
     * Fill environment variables.
     *
     * @param  array                                             $env $_SERVER environment.
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\JsonFile
     * @throws \RuntimeException
     */
    public function fillJobs(array $env)
    {
        return $this
        ->fillStandardizedEnvVars($env)
        ->ensureJobs();
    }

    /**
     * Exclude source files that have no executable statements.
     *
     * @return void
     */
    public function excludeNoStatementsFiles()
    {
        $this->sourceFiles = array_filter(
            $this->sourceFiles,
            function (SourceFile $sourceFile) {
                return $sourceFile->getMetrics()->hasStatements();
            }
        );
    }

    /**
     * Sort source files by path.
     *
     * @return void
     */
    public function sortSourceFiles()
    {
        ksort($this->sourceFiles);
    }

    /**
     * Return line coverage.
     *
     * @return float
     */
    public function reportLineCoverage()
    {
        $metrics = $this->getMetrics();

        foreach ($this->sourceFiles as $sourceFile) {
            /* @var $sourceFile \Contrib\Bundle\CoverallsV1Bundle\Entity\SourceFile */
            $metrics->merge($sourceFile->getMetrics());
        }

        return $metrics->getLineCoverage();
    }

    // internal method

    /**
     * Convert to json property.
     *
     * @param  mixed $prop
     * @return mixed
     */
    protected function toJsonProperty($prop)
    {
        if ($prop instanceof Coveralls) {
            return $prop->toArray();
        } elseif (is_array($prop)) {
            return $this->toJsonPropertyArray($prop);
        }

        return $prop;
    }

    /**
     * Convert to array as json property.
     *
     * @param  array $propArray
     * @return array
     */
    protected function toJsonPropertyArray(array $propArray)
    {
        $array = array();

        foreach ($propArray as $prop) {
            $array[] = $this->toJsonProperty($prop);
        }

        return $array;
    }

    /**
     * Fill standardized environment variables.
     *
     * "CI_NAME", "CI_BUILD_NUMBER" must be set.
     *
     * Env vars are:
     *
     * * CI_NAME
     * * CI_BUILD_NUMBER
     * * CI_BUILD_URL
     * * CI_BRANCH
     * * CI_PULL_REQUEST
     *
     * These vars are supported by Codeship.
     *
     * @param  array                                             $env $_SERVER environment.
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\JsonFile
     */
    protected function fillStandardizedEnvVars(array $env)
    {
        $map = array(
            // defined in Ruby lib
            'serviceName'        => 'CI_NAME',
            'serviceNumber'      => 'CI_BUILD_NUMBER',
            'serviceBuildUrl'    => 'CI_BUILD_URL',
            'serviceBranch'      => 'CI_BRANCH',
            'servicePullRequest' => 'CI_PULL_REQUEST',

            // extends by php-coveralls
            'serviceJobId'       => 'CI_JOB_ID',
            'serviceEventType'   => 'COVERALLS_EVENT_TYPE',
            'repoToken'          => 'COVERALLS_REPO_TOKEN',
        );

        foreach ($map as $propName => $envName) {
            if (isset($env[$envName])) {
                $this->$propName = $env[$envName];
            }
        }

        return $this;
    }

    /**
     * Ensure data consistency for jobs API.
     *
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\JsonFile
     * @throws \RuntimeException
     */
    protected function ensureJobs()
    {
        if (!$this->hasSourceFiles()) {
            throw new \RuntimeException('source_files must be set');
        }

        if ($this->requireServiceJobId()) {
            return $this;
        }

        if ($this->requireServiceNumber()) {
            return $this;
        }

        if ($this->requireServiceEventType()) {
            return $this;
        }

        if ($this->isUnsupportedServiceJob()) {
            return $this;
        }

        $message = 'requirements are not satisfied.';

        throw new \RuntimeException($message);
    }

    /**
     * Return whether the job requires "service_job_id" (for Travis CI).
     *
     * @return boolean
     */
    protected function requireServiceJobId()
    {
        return isset($this->serviceName) && isset($this->serviceJobId) && !isset($this->repoToken);
    }

    /**
     * Return whether the job requires "service_number" (for CircleCI, Jenkins, Codeship or other CIs).
     *
     * @return boolean
     */
    protected function requireServiceNumber()
    {
        return isset($this->serviceName) && isset($this->serviceNumber) && isset($this->repoToken);
    }

    /**
     * Return whether the job requires "service_event_type" (for local environment).
     *
     * @return boolean
     */
    protected function requireServiceEventType()
    {
        return isset($this->serviceName) && isset($this->serviceEventType) && isset($this->repoToken);
    }

    /**
     * Return whether the job is running on unsupported service.
     *
     * @return boolean
     */
    protected function isUnsupportedServiceJob()
    {
        return !isset($this->serviceJobId) && !isset($this->serviceNumber) && !isset($this->serviceEventType) && isset($this->repoToken);
    }

    // accessor

    /**
     * Return whether the json file has source file.
     *
     * @param  string  $path Absolute path to source file.
     * @return boolean
     */
    public function hasSourceFile($path)
    {
        return isset($this->sourceFiles[$path]);
    }

    /**
     * Return source file.
     *
     * @param  string                                                   $path Absolute path to source file.
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\SourceFile|null
     */
    public function getSourceFile($path)
    {
        if ($this->hasSourceFile($path)) {
            return $this->sourceFiles[$path];
        }

        return null;
    }

    /**
     * Add source file.
     *
     * @param SourceFile $sourceFile
     */
    public function addSourceFile(SourceFile $sourceFile)
    {
        $this->sourceFiles[$sourceFile->getPath()] = $sourceFile;
    }

    /**
     * Return whether the json file has a source file.
     *
     * @return boolean
     */
    public function hasSourceFiles()
    {
        return count($this->sourceFiles) > 0;
    }

    /**
     * Return source files.
     *
     * @return SourceFile[]
     */
    public function getSourceFiles()
    {
        return $this->sourceFiles;
    }

    /**
     * Set service name.
     *
     * @param  string    $serviceName Service name.
     * @return Coveralls
     */
    public function setServiceName($serviceName)
    {
        $this->serviceName = $serviceName;

        return $this;
    }

    /**
     * Return service name.
     *
     * @return string
     */
    public function getServiceName()
    {
        if (isset($this->serviceName)) {
            return $this->serviceName;
        }

        return null;
    }

    /**
     * Set repository token.
     *
     * @param  string    $repoToken Repository token.
     * @return Coveralls
     */
    public function setRepoToken($repoToken)
    {
        $this->repoToken = $repoToken;

        return $this;
    }

    /**
     * Return repository token.
     *
     * @return string
     */
    public function getRepoToken()
    {
        if (isset($this->repoToken)) {
            return $this->repoToken;
        }

        return null;
    }

    /**
     * Set service job id.
     *
     * @param  string    $serviceJobId Service job id.
     * @return Coveralls
     */
    public function setServiceJobId($serviceJobId)
    {
        $this->serviceJobId = $serviceJobId;

        return $this;
    }

    /**
     * Return service job id.
     *
     * @return string
     */
    public function getServiceJobId()
    {
        if (isset($this->serviceJobId)) {
            return $this->serviceJobId;
        }

        return null;
    }

    /**
     * Return service number.
     *
     * @return string
     */
    public function getServiceNumber()
    {
        return $this->serviceNumber;
    }

    /**
     * Return service event type.
     *
     * @return string
     */
    public function getServiceEventType()
    {
        return $this->serviceEventType;
    }

    /**
     * Return build URL of the project.
     *
     * @return string
     */
    public function getServiceBuildUrl()
    {
        return $this->serviceBuildUrl;
    }

    /**
     * Return branch name.
     *
     * @return string
     */
    public function getServiceBranch()
    {
        return $this->serviceBranch;
    }

    /**
     * Return pull request info.
     *
     * @return string
     */
    public function getServicePullRequest()
    {
        return $this->servicePullRequest;
    }

    /**
     * Set git data.
     *
     * @param  array     $git Git data.
     * @return Coveralls
     */
    public function setGit(Git $git)
    {
        $this->git = $git;

        return $this;
    }

    /**
     * Return git data.
     *
     * @return array
     */
    public function getGit()
    {
        if (isset($this->git)) {
            return $this->git;
        }

        return null;
    }

    /**
     * Set timestamp when the job ran.
     *
     * @param  string    $runAt Timestamp.
     * @return Coveralls
     */
    public function setRunAt($runAt)
    {
        $this->runAt = $runAt;

        return $this;
    }

    /**
     * Return timestamp when the job ran.
     *
     * @return string
     */
    public function getRunAt()
    {
        if (isset($this->runAt)) {
            return $this->runAt;
        }

        return null;
    }

    /**
     * Return metrics.
     *
     * @return \Contrib\Bundle\CoverallsV1Bundle\Entity\Metrics
     */
    public function getMetrics()
    {
        if (!isset($this->metrics)) {
            $this->metrics = new Metrics();
        }

        return $this->metrics;
    }
}
