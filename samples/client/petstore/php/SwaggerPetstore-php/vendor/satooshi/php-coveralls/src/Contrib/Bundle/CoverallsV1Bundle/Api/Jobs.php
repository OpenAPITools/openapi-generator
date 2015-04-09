<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Api;

use Contrib\Bundle\CoverallsV1Bundle\Entity\JsonFile;
use Contrib\Bundle\CoverallsV1Bundle\Collector\CloverXmlCoverageCollector;
use Contrib\Bundle\CoverallsV1Bundle\Collector\GitInfoCollector;
use Contrib\Bundle\CoverallsV1Bundle\Collector\CiEnvVarsCollector;
use Contrib\Component\System\Git\GitCommand;

/**
 * Jobs API.
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class Jobs extends CoverallsApi
{
    /**
     * URL for jobs API.
     *
     * @var string
     */
    const URL = 'https://coveralls.io/api/v1/jobs';

    /**
     * Filename as a POST parameter.
     *
     * @var string
     */
    const FILENAME = 'json_file';

    /**
     * JsonFile.
     *
     * @var Contrib\Bundle\CoverallsV1Bundle\Entity\JsonFile
     */
    protected $jsonFile;

    // API

    /**
     * Collect clover XML into json_file.
     *
     * @return \Contrib\Bundle\CoverallsV1Bundle\Api\Jobs
     */
    public function collectCloverXml()
    {
        $srcDir         = $this->config->getSrcDir();
        $cloverXmlPaths = $this->config->getCloverXmlPaths();
        $xmlCollector   = new CloverXmlCoverageCollector();

        foreach ($cloverXmlPaths as $cloverXmlPath) {
            $xml = simplexml_load_file($cloverXmlPath);

            $xmlCollector->collect($xml, $srcDir);
        }

        $this->jsonFile = $xmlCollector->getJsonFile();

        if ($this->config->isExcludeNoStatements()) {
            $this->jsonFile->excludeNoStatementsFiles();
        }

        $this->jsonFile->sortSourceFiles();

        return $this;
    }

    /**
     * Collect git repository info into json_file.
     *
     * @return \Contrib\Bundle\CoverallsV1Bundle\Api\Jobs
     */
    public function collectGitInfo()
    {
        $command      = new GitCommand();
        $gitCollector = new GitInfoCollector($command);

        $this->jsonFile->setGit($gitCollector->collect());

        return $this;
    }

    /**
     * Collect environment variables.
     *
     * @param  array                                      $env $_SERVER environment.
     * @return \Contrib\Bundle\CoverallsV1Bundle\Api\Jobs
     */
    public function collectEnvVars(array $env)
    {
        $envCollector = new CiEnvVarsCollector($this->config);

        $this->jsonFile->fillJobs($envCollector->collect($env));

        return $this;
    }

    /**
     * Dump uploading json file.
     *
     * @return \Contrib\Bundle\CoverallsV1Bundle\Api\Jobs
     */
    public function dumpJsonFile()
    {
        $jsonPath = $this->config->getJsonPath();

        file_put_contents($jsonPath, $this->jsonFile);

        return $this;
    }

    /**
     * Send json_file to jobs API.
     *
     * @return \Guzzle\Http\Message\Response|null
     * @throws \RuntimeException
     */
    public function send()
    {
        if ($this->config->isDryRun()) {
            return;
        }

        $jsonPath = $this->config->getJsonPath();

        return $this->upload(static::URL, $jsonPath, static::FILENAME);
    }

    // internal method

    /**
     * Upload a file.
     *
     * @param  string                        $url      URL to upload.
     * @param  string                        $path     File path.
     * @param  string                        $filename Filename.
     * @return \Guzzle\Http\Message\Response Response.
     * @throws \RuntimeException
     */
    protected function upload($url, $path, $filename)
    {
        $request  = $this->client->post($url)->addPostFiles(array($filename => $path));

        return $request->send();
    }

    // accessor

    /**
     * Set JsonFile.
     *
     * @param  JsonFile                                   $jsonFile json_file content.
     * @return \Contrib\Bundle\CoverallsV1Bundle\Api\Jobs
     */
    public function setJsonFile(JsonFile $jsonFile)
    {
        $this->jsonFile = $jsonFile;

        return $this;
    }

    /**
     * Return JsonFile.
     *
     * @return JsonFile
     */
    public function getJsonFile()
    {
        if (isset($this->jsonFile)) {
            return $this->jsonFile;
        }

        return null;
    }
}
