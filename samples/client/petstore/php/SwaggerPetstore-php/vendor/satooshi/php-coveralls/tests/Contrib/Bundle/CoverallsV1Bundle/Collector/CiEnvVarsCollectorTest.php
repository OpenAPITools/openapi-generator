<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Collector;

use Contrib\Bundle\CoverallsV1Bundle\Config\Configuration;

/**
 * @covers Contrib\Bundle\CoverallsV1Bundle\Collector\CiEnvVarsCollector
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class CiEnvVarsCollectorTest extends \PHPUnit_Framework_TestCase
{
    protected function setUp()
    {
        $this->dir = realpath(__DIR__ . '/../../../../');
        $this->rootDir = realpath($this->dir . '/prj/files');
        $this->srcDir = $this->rootDir;
        //$this->url           = 'https://coveralls.io/api/v1/jobs';
        //$this->jsonPath      = __DIR__ . '/coveralls.json';
        //$this->filename      = 'json_file';
        $this->cloverXmlPath = $this->rootDir . 'clover.xml';
    }

    protected function createConfiguration()
    {
        $config = new Configuration($this->rootDir);

        return $config->setSrcDir($this->srcDir)->addCloverXmlPath($this->cloverXmlPath);
    }

    protected function createCiEnvVarsCollector($config = null)
    {
        if ($config === null) {
            $config = $this->createConfiguration();
        }

        return new CiEnvVarsCollector($config);
    }

    // collect()

    /**
     * @test
     */
    public function collectTravisCiEnvVars()
    {
        $serviceName  = 'travis-ci';
        $serviceJobId = '1.1';

        $env = array();
        $env['TRAVIS']        = true;
        $env['TRAVIS_JOB_ID'] = $serviceJobId;

        $object = $this->createCiEnvVarsCollector();

        $actual = $object->collect($env);

        $this->assertArrayHasKey('CI_NAME', $actual);
        $this->assertEquals($serviceName, $actual['CI_NAME']);

        $this->assertArrayHasKey('CI_JOB_ID', $actual);
        $this->assertEquals($serviceJobId, $actual['CI_JOB_ID']);
    }

    /**
     * @test
     */
    public function collectTravisProEnvVars()
    {
        $serviceName  = 'travis-pro';
        $serviceJobId = '1.2';

        $env = array();
        $env['TRAVIS']        = true;
        $env['TRAVIS_JOB_ID'] = $serviceJobId;

        $config = $this->createConfiguration();
        $config->setServiceName($serviceName);

        $object = $this->createCiEnvVarsCollector($config);

        $actual = $object->collect($env);

        $this->assertArrayHasKey('CI_NAME', $actual);
        $this->assertEquals($serviceName, $actual['CI_NAME']);

        $this->assertArrayHasKey('CI_JOB_ID', $actual);
        $this->assertEquals($serviceJobId, $actual['CI_JOB_ID']);
    }

    /**
     * @test
     */
    public function collectCircleCiEnvVars()
    {
        $serviceName   = 'circleci';
        $serviceNumber = '123';

        $env = array();
        $env['COVERALLS_REPO_TOKEN'] = 'token';
        $env['CIRCLECI']             = 'true';
        $env['CIRCLE_BUILD_NUM']     = $serviceNumber;

        $object = $this->createCiEnvVarsCollector();

        $actual = $object->collect($env);

        $this->assertArrayHasKey('CI_NAME', $actual);
        $this->assertEquals($serviceName, $actual['CI_NAME']);

        $this->assertArrayHasKey('CI_BUILD_NUMBER', $actual);
        $this->assertEquals($serviceNumber, $actual['CI_BUILD_NUMBER']);
    }

    /**
     * @test
     */
    public function collectJenkinsEnvVars()
    {
        $serviceName   = 'jenkins';
        $serviceNumber = '123';
        $buildUrl      = 'http://localhost:8080';

        $env = array();
        $env['COVERALLS_REPO_TOKEN'] = 'token';
        $env['JENKINS_URL']          = $buildUrl;
        $env['BUILD_NUMBER']         = $serviceNumber;

        $object = $this->createCiEnvVarsCollector();

        $actual = $object->collect($env);

        $this->assertArrayHasKey('CI_NAME', $actual);
        $this->assertEquals($serviceName, $actual['CI_NAME']);

        $this->assertArrayHasKey('CI_BUILD_NUMBER', $actual);
        $this->assertEquals($serviceNumber, $actual['CI_BUILD_NUMBER']);

        $this->assertArrayHasKey('CI_BUILD_URL', $actual);
        $this->assertEquals($buildUrl, $actual['CI_BUILD_URL']);
    }

    /**
     * @test
     */
    public function collectLocalEnvVars()
    {
        $serviceName      = 'php-coveralls';
        $serviceEventType = 'manual';

        $env = array();
        $env['COVERALLS_REPO_TOKEN']  = 'token';
        $env['COVERALLS_RUN_LOCALLY'] = '1';

        $object = $this->createCiEnvVarsCollector();

        $actual = $object->collect($env);

        $this->assertArrayHasKey('CI_NAME', $actual);
        $this->assertEquals($serviceName, $actual['CI_NAME']);

        $this->assertArrayHasKey('COVERALLS_EVENT_TYPE', $actual);
        $this->assertEquals($serviceEventType, $actual['COVERALLS_EVENT_TYPE']);

        $this->assertArrayHasKey('CI_JOB_ID', $actual);
        $this->assertNull($actual['CI_JOB_ID']);
    }

    /**
     * @test
     */
    public function collectUnsupportedConfig()
    {
        $repoToken = 'token';

        $env = array();

        $config = $this->createConfiguration();
        $config->setRepoToken($repoToken);

        $object = $this->createCiEnvVarsCollector($config);

        $actual = $object->collect($env);

        $this->assertArrayHasKey('COVERALLS_REPO_TOKEN', $actual);
        $this->assertEquals($repoToken, $actual['COVERALLS_REPO_TOKEN']);
    }

    /**
     * @test
     */
    public function collectUnsupportedEnvVars()
    {
        $repoToken = 'token';

        $env = array();
        $env['COVERALLS_REPO_TOKEN'] = $repoToken;

        $object = $this->createCiEnvVarsCollector();

        $actual = $object->collect($env);

        $this->assertArrayHasKey('COVERALLS_REPO_TOKEN', $actual);
        $this->assertEquals($repoToken, $actual['COVERALLS_REPO_TOKEN']);
    }
}
