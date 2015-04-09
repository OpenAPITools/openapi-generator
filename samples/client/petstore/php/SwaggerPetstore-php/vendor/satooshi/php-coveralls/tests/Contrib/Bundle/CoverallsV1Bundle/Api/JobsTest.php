<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Api;

use Contrib\Bundle\CoverallsV1Bundle\Entity\JsonFile;
use Contrib\Bundle\CoverallsV1Bundle\Config\Configuration;
use Contrib\Bundle\CoverallsV1Bundle\Collector\CloverXmlCoverageCollector;
use Contrib\Bundle\CoverallsV1Bundle\Collector\CiEnvVarsCollector;

/**
 * @covers Contrib\Bundle\CoverallsV1Bundle\Api\Jobs
 * @covers Contrib\Bundle\CoverallsV1Bundle\Api\CoverallsApi
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class JobsTest extends \PHPUnit_Framework_TestCase
{
    protected function setUp()
    {
        $this->dir           = realpath(__DIR__ . '/../../../../');
        $this->rootDir       = realpath($this->dir . '/prj/files');
        $this->srcDir        = $this->rootDir;
        $this->url           = 'https://coveralls.io/api/v1/jobs';
        $this->jsonPath      = __DIR__ . '/coveralls.json';
        $this->filename      = 'json_file';
        $this->cloverXmlPath = $this->rootDir . 'clover.xml';
    }

    protected function tearDown()
    {
        $this->rmFile($this->jsonPath);
        $this->rmFile($this->cloverXmlPath);
    }

    protected function rmFile($file)
    {
        if (is_file($file)) {
            unlink($file);
        }
    }

    protected function createJobsWith()
    {
        $this->config = new Configuration($this->rootDir);

        $this->config
        ->setJsonPath($this->jsonPath)
        ->setDryRun(false);

        $this->client = $this->createAdapterMockWith($this->url, $this->filename, $this->jsonPath);

        return new Jobs($this->config, $this->client);
    }

    protected function createJobsNeverSend()
    {
        $this->config = new Configuration($this->rootDir);
        $this->config
        ->setJsonPath($this->jsonPath)
        ->setDryRun(false);

        $this->client = $this->createAdapterMockNeverCalled();

        return new Jobs($this->config, $this->client);
    }

    protected function createJobsNeverSendOnDryRun()
    {
        $this->config = new Configuration($this->rootDir);
        $this->config
        ->setJsonPath($this->jsonPath)
        ->setDryRun(true);

        $this->client = $this->createAdapterMockNeverCalled();

        return new Jobs($this->config, $this->client);
    }


    protected function createAdapterMockNeverCalled()
    {
        $client = $this->getMock('Guzzle\Http\Client', array('send'));

        $client
        ->expects($this->never())
        ->method('send');

        return $client;
    }

    protected function createAdapterMockWith($url, $filename, $jsonPath)
    {
        $client = $this->getMock('Guzzle\Http\Client', array('post', 'addPostFiles'));
        $request = $this->getMockBuilder('Guzzle\Http\Message\EntityEnclosingRequest')
        ->disableOriginalConstructor()
        ->getMock();

        $client
        ->expects($this->once())
        ->method('post')
        ->with($this->equalTo($url))
        ->will($this->returnSelf());

        $client
        ->expects($this->once())
        ->method('addPostFiles')
        ->with($this->equalTo(array($filename => $jsonPath)))
        ->will($this->returnValue($request));

        $request
        ->expects($this->once())
        ->method('send')
        ->with()
        ;

        return $client;
    }

    protected function createConfiguration()
    {
        $config = new Configuration($this->rootDir);

        return $config
        ->setSrcDir($this->srcDir)
        ->addCloverXmlPath($this->cloverXmlPath);
    }

    protected function getCloverXml()
    {
        $xml = <<<XML
<?xml version="1.0" encoding="UTF-8"?>
<coverage generated="1365848893">
  <project timestamp="1365848893">
    <file name="%s/test.php">
      <class name="TestFile" namespace="global">
        <metrics methods="1" coveredmethods="0" conditionals="0" coveredconditionals="0" statements="1" coveredstatements="0" elements="2" coveredelements="0"/>
      </class>
      <line num="5" type="method" name="__construct" crap="1" count="0"/>
      <line num="7" type="stmt" count="0"/>
    </file>
    <file name="%s/TestInterface.php">
      <class name="TestInterface" namespace="global">
        <metrics methods="1" coveredmethods="0" conditionals="0" coveredconditionals="0" statements="0" coveredstatements="0" elements="1" coveredelements="0"/>
      </class>
      <line num="5" type="method" name="hello" crap="1" count="0"/>
    </file>
    <file name="%s/AbstractClass.php">
      <class name="AbstractClass" namespace="global">
        <metrics methods="1" coveredmethods="0" conditionals="0" coveredconditionals="0" statements="0" coveredstatements="0" elements="1" coveredelements="0"/>
      </class>
      <line num="5" type="method" name="hello" crap="1" count="0"/>
    </file>
    <file name="dummy.php">
      <class name="TestFile" namespace="global">
        <metrics methods="1" coveredmethods="0" conditionals="0" coveredconditionals="0" statements="1" coveredstatements="0" elements="2" coveredelements="0"/>
      </class>
      <line num="5" type="method" name="__construct" crap="1" count="0"/>
      <line num="7" type="stmt" count="0"/>
    </file>
    <package name="Hoge">
      <file name="%s/test2.php">
        <class name="TestFile" namespace="Hoge">
          <metrics methods="1" coveredmethods="0" conditionals="0" coveredconditionals="0" statements="1" coveredstatements="0" elements="2" coveredelements="0"/>
        </class>
        <line num="6" type="method" name="__construct" crap="1" count="0"/>
        <line num="8" type="stmt" count="0"/>
      </file>
    </package>
  </project>
</coverage>
XML;
        return sprintf($xml, $this->rootDir, $this->rootDir, $this->rootDir, $this->rootDir);
    }

    protected function createCloverXml()
    {
        $xml = $this->getCloverXml();

        return simplexml_load_string($xml);
    }

    protected function getNoSourceCloverXml()
    {
        return <<<XML
<?xml version="1.0" encoding="UTF-8"?>
<coverage generated="1365848893">
  <project timestamp="1365848893">
    <file name="dummy.php">
      <class name="TestFile" namespace="global">
        <metrics methods="1" coveredmethods="0" conditionals="0" coveredconditionals="0" statements="1" coveredstatements="0" elements="2" coveredelements="0"/>
      </class>
      <line num="5" type="method" name="__construct" crap="1" count="0"/>
      <line num="7" type="stmt" count="0"/>
    </file>
  </project>
</coverage>
XML;
    }

    protected function createNoSourceCloverXml()
    {
        $xml = $this->getNoSourceCloverXml();

        return simplexml_load_string($xml);
    }

    protected function collectJsonFile()
    {
        $xml       = $this->createCloverXml();
        $collector = new CloverXmlCoverageCollector();

        return $collector->collect($xml, $this->rootDir);
    }

    protected function collectJsonFileWithoutSourceFiles()
    {
        $xml       = $this->createNoSourceCloverXml();
        $collector = new CloverXmlCoverageCollector();

        return $collector->collect($xml, $this->rootDir);
    }

    protected function createCiEnvVarsCollector($config = null)
    {
        if ($config === null) {
            $config = $this->createConfiguration();
        }

        return new CiEnvVarsCollector($config);
    }

    // getJsonFile()

    /**
     * @test
     */
    public function shouldNotHaveJsonFileOnConstruction()
    {
        $object = $this->createJobsNeverSendOnDryRun();

        $this->assertNull($object->getJsonFile());
    }

    // setJsonFile()

    /**
     * @test
     */
    public function setJsonFile()
    {
        $jsonFile = $this->collectJsonFile();

        $object = $this->createJobsNeverSendOnDryRun()->setJsonFile($jsonFile);

        $this->assertSame($jsonFile, $object->getJsonFile());
    }

    // getConfiguration()

    /**
     * @test
     */
    public function getConfiguration()
    {
        $config = $this->createConfiguration();

        $object = new Jobs($config);

        $this->assertSame($config, $object->getConfiguration());
    }

    // getHttpClient()

    /**
     * @test
     */
    public function shouldNotHaveHttpClientOnConstructionWithoutHttpClient()
    {
        $config = $this->createConfiguration();

        $object = new Jobs($config);

        $this->assertNull($object->getHttpClient());
    }

    /**
     * @test
     */
    public function shouldHaveHttpClientOnConstructionWithHttpClient()
    {
        $config = $this->createConfiguration();
        $client = $this->createAdapterMockNeverCalled();

        $object = new Jobs($config, $client);

        $this->assertSame($client, $object->getHttpClient());
    }

    // setHttpClient()

    /**
     * @test
     */
    public function setHttpClient()
    {
        $config = $this->createConfiguration();
        $client = $this->createAdapterMockNeverCalled();

        $object = new Jobs($config);
        $object->setHttpClient($client);

        $this->assertSame($client, $object->getHttpClient());
    }

    // collectCloverXml()

    /**
     * @test
     */
    public function collectCloverXml()
    {
        $xml = $this->getCloverXml();

        file_put_contents($this->cloverXmlPath, $xml);

        $config = $this->createConfiguration();

        $object = new Jobs($config);

        $same = $object->collectCloverXml();

        // return $this
        $this->assertSame($same, $object);

        return $object;
    }

    /**
     * @test
     * @depends collectCloverXml
     */
    public function shouldHaveJsonFileAfterCollectCloverXml(Jobs $object)
    {
        $jsonFile = $object->getJsonFile();

        $this->assertNotNull($jsonFile);
        $sourceFiles = $jsonFile->getSourceFiles();
        $this->assertCount(4, $sourceFiles);

        return $jsonFile;
    }

    /**
     * @test
     * @depends shouldHaveJsonFileAfterCollectCloverXml
     */
    public function shouldNotHaveGitAfterCollectCloverXml(JsonFile $jsonFile)
    {
        $git = $jsonFile->getGit();

        $this->assertNull($git);
    }

    /**
     * @test
     */
    public function collectCloverXmlExcludingNoStatementsFiles()
    {
        $xml = $this->getCloverXml();

        file_put_contents($this->cloverXmlPath, $xml);

        $config = $this->createConfiguration()->setExcludeNoStatements(true);

        $object = new Jobs($config);

        $same = $object->collectCloverXml();

        // return $this
        $this->assertSame($same, $object);

        return $object;
    }

    /**
     * @test
     * @depends collectCloverXmlExcludingNoStatementsFiles
     */
    public function shouldHaveJsonFileAfterCollectCloverXmlExcludingNoStatementsFiles(Jobs $object)
    {
        $jsonFile = $object->getJsonFile();

        $this->assertNotNull($jsonFile);
        $sourceFiles = $jsonFile->getSourceFiles();
        $this->assertCount(2, $sourceFiles);

        return $jsonFile;
    }

    // collectGitInfo()

    /**
     * @test
     * @depends collectCloverXml
     */
    public function collectGitInfo(Jobs $object)
    {
        $same = $object->collectGitInfo();

        // return $this
        $this->assertSame($same, $object);

        return $object;
    }

    /**
     * @test
     * @depends collectGitInfo
     */
    public function shouldHaveJsonFileAfterCollectGitInfo(Jobs $object)
    {
        $jsonFile = $object->getJsonFile();

        $this->assertNotNull($jsonFile);

        return $jsonFile;
    }

    /**
     * @test
     * @depends shouldHaveJsonFileAfterCollectGitInfo
     */
    public function shouldHaveGitAfterCollectGitInfo(JsonFile $jsonFile)
    {
        $git = $jsonFile->getGit();

        $this->assertNotNull($git);
    }

    // send()

    /**
     * @test
     */
    public function sendTravisCiJob()
    {
        $serviceName  = 'travis-ci';
        $serviceJobId = '1.1';

        $server = array();
        $server['TRAVIS']        = true;
        $server['TRAVIS_JOB_ID'] = $serviceJobId;

        $object   = $this->createJobsWith();
        $jsonFile = $this->collectJsonFile();

        $object
        ->setJsonFile($jsonFile)
        ->collectEnvVars($server)
        ->dumpJsonFile()
        ->send();
    }

    /**
     * @test
     */
    public function sendTravisProJob()
    {
        $serviceName  = 'travis-pro';
        $serviceJobId = '1.1';

        $server = array();
        $server['TRAVIS']        = true;
        $server['TRAVIS_JOB_ID'] = $serviceJobId;

        $object   = $this->createJobsWith();
        $config   = $object->getConfiguration()->setServiceName($serviceName);
        $jsonFile = $this->collectJsonFile();

        $object
        ->setJsonFile($jsonFile)
        ->collectEnvVars($server)
        ->dumpJsonFile()
        ->send();

        $this->assertEquals($serviceName, $jsonFile->getServiceName());
        $this->assertEquals($serviceJobId, $jsonFile->getServiceJobId());
    }

    /**
     * @test
     */
    public function sendCircleCiJob()
    {
        $serviceName   = 'circleci';
        $serviceNumber = '123';
        $repoToken     = 'token';

        $server = array();
        $server['COVERALLS_REPO_TOKEN'] = $repoToken;
        $server['CIRCLECI']             = 'true';
        $server['CIRCLE_BUILD_NUM']     = $serviceNumber;

        $object   = $this->createJobsWith();
        $jsonFile = $this->collectJsonFile();

        $object
        ->setJsonFile($jsonFile)
        ->collectEnvVars($server)
        ->dumpJsonFile()
        ->send();
    }

    /**
     * @test
     */
    public function sendJenkinsJob()
    {
        $serviceName   = 'jenkins';
        $serviceNumber = '123';
        $repoToken     = 'token';

        $server = array();
        $server['COVERALLS_REPO_TOKEN'] = $repoToken;
        $server['JENKINS_URL']          = 'http://localhost:8080';
        $server['BUILD_NUMBER']         = $serviceNumber;

        $object   = $this->createJobsWith();
        $jsonFile = $this->collectJsonFile();

        $object
        ->setJsonFile($jsonFile)
        ->collectEnvVars($server)
        ->dumpJsonFile()
        ->send();
    }

    /**
     * @test
     */
    public function sendLocalJob()
    {
        $serviceName      = 'php-coveralls';
        $serviceEventType = 'manual';

        $server = array();
        $server['COVERALLS_RUN_LOCALLY'] = '1';

        $object   = $this->createJobsWith();
        $config   = $object->getConfiguration()->setRepoToken('token');
        $jsonFile = $this->collectJsonFile();

        $object
        ->setJsonFile($jsonFile)
        ->collectEnvVars($server)
        ->dumpJsonFile()
        ->send();
    }

    /**
     * @test
     */
    public function sendUnsupportedJob()
    {
        $server = array();
        $server['COVERALLS_REPO_TOKEN'] = 'token';

        $object   = $this->createJobsWith();
        $jsonFile = $this->collectJsonFile();

        $object
        ->setJsonFile($jsonFile)
        ->collectEnvVars($server)
        ->dumpJsonFile()
        ->send();
    }

    /**
     * @test
     */
    public function sendUnsupportedGitJob()
    {
        $server = array();
        $server['COVERALLS_REPO_TOKEN'] = 'token';
        $server['GIT_COMMIT']           = 'abc123';

        $object   = $this->createJobsWith();
        $jsonFile = $this->collectJsonFile();

        $object
        ->setJsonFile($jsonFile)
        ->collectEnvVars($server)
        ->dumpJsonFile()
        ->send();
    }

    /**
     * @test
     */
    public function shouldNotSendJobIfTestEnv()
    {
        $server = array();
        $server['TRAVIS']        = true;
        $server['TRAVIS_JOB_ID'] = '1.1';

        $object   = $this->createJobsNeverSendOnDryRun();
        $config   = $object->getConfiguration()->setEnv('test');
        $jsonFile = $this->collectJsonFile();

        $object
        ->setJsonFile($jsonFile)
        ->collectEnvVars($server)
        ->dumpJsonFile()
        ->send();
    }

    /**
     * @test
     * @expectedException RuntimeException
     */
    public function throwRuntimeExceptionIfInvalidEnv()
    {
        $server = array();

        $object   = $this->createJobsNeverSend();
        $jsonFile = $this->collectJsonFile();

        $object
        ->setJsonFile($jsonFile)
        ->collectEnvVars($server)
        ->dumpJsonFile()
        ->send();
    }

    /**
     * @test
     * @expectedException RuntimeException
     */
    public function throwRuntimeExceptionIfNoSourceFiles()
    {
        $server = array();
        $server['TRAVIS']               = true;
        $server['TRAVIS_JOB_ID']        = '1.1';
        $server['COVERALLS_REPO_TOKEN'] = 'token';
        $server['GIT_COMMIT']           = 'abc123';

        $object   = $this->createJobsNeverSend();
        $jsonFile = $this->collectJsonFile();

        $object
        ->setJsonFile($jsonFile)
        ->collectEnvVars($server)
        ->dumpJsonFile()
        ->send();
    }
}
