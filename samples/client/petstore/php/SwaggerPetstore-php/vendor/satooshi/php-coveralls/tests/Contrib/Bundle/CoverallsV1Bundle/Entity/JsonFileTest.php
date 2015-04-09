<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Entity;

use Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Remote;
use Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Commit;
use Contrib\Bundle\CoverallsV1Bundle\Entity\Git\Git;
use Contrib\Bundle\CoverallsV1Bundle\Collector\CloverXmlCoverageCollector;

/**
 * @covers Contrib\Bundle\CoverallsV1Bundle\Entity\JsonFile
 * @covers Contrib\Bundle\CoverallsV1Bundle\Entity\Coveralls
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class JsonFileTest extends \PHPUnit_Framework_TestCase
{
    protected function setUp()
    {
        $this->dir      = realpath(__DIR__ . '/../../../../');
        $this->rootDir  = realpath($this->dir . '/prj/files');
        $this->filename = 'test.php';
        $this->path     = $this->rootDir . DIRECTORY_SEPARATOR . $this->filename;

        $this->object = new JsonFile();
    }


    protected function createSourceFile()
    {
        return new SourceFile($this->path, $this->filename);
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
      <line num="7" type="stmt" count="1"/>
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

    protected function collectJsonFile()
    {
        $xml       = $this->createCloverXml();
        $collector = new CloverXmlCoverageCollector();

        return $collector->collect($xml, $this->rootDir);
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

    protected function collectJsonFileWithoutSourceFiles()
    {
        $xml       = $this->createNoSourceCloverXml();
        $collector = new CloverXmlCoverageCollector();

        return $collector->collect($xml, $this->rootDir);
    }


    // hasSourceFile()
    // getSourceFile()

    /**
     * @test
     */
    public function shouldNotHaveSourceFileOnConstruction()
    {
        $path = 'test.php';

        $this->assertFalse($this->object->hasSourceFile($path));
        $this->assertNull($this->object->getSourceFile($path));
    }

    // hasSourceFiles()
    // getSourceFiles()

    /**
     * @test
     */
    public function countZeroSourceFilesOnConstruction()
    {
        $this->assertFalse($this->object->hasSourceFiles());
        $this->assertEmpty($this->object->getSourceFiles());
    }

    // getServiceName()

    /**
     * @test
     */
    public function shouldNotHaveServiceNameOnConstruction()
    {
        $this->assertNull($this->object->getServiceName());
    }

    // getRepoToken()

    /**
     * @test
     */
    public function shouldNotHaveRepoTokenOnConstruction()
    {
        $this->assertNull($this->object->getRepoToken());
    }

    // getServiceJobId()

    /**
     * @test
     */
    public function shouldNotHaveServiceJobIdOnConstruction()
    {
        $this->assertNull($this->object->getServiceJobId());
    }

    // getServiceNumber()

    /**
     * @test
     */
    public function shouldNotHaveServiceNumberOnConstruction()
    {
        $this->assertNull($this->object->getServiceNumber());
    }

    // getServiceEventType()

    /**
     * @test
     */
    public function shouldNotHaveServiceEventTypeOnConstruction()
    {
        $this->assertNull($this->object->getServiceEventType());
    }

    // getServiceBuildUrl()

    /**
     * @test
     */
    public function shouldNotHaveServiceBuildUrlOnConstruction()
    {
        $this->assertNull($this->object->getServiceBuildUrl());
    }

    // getServiceBranch()

    /**
     * @test
     */
    public function shouldNotHaveServiceBranchOnConstruction()
    {
        $this->assertNull($this->object->getServiceBranch());
    }

    // getServicePullRequest()

    /**
     * @test
     */
    public function shouldNotHaveServicePullRequestOnConstruction()
    {
        $this->assertNull($this->object->getServicePullRequest());
    }

    // getGit()

    /**
     * @test
     */
    public function shouldNotHaveGitOnConstruction()
    {
        $this->assertNull($this->object->getGit());
    }

    // getRunAt()

    /**
     * @test
     */
    public function shouldNotHaveRunAtOnConstruction()
    {
        $this->assertNull($this->object->getRunAt());
    }

    // getMetrics()

    /**
     * @test
     */
    public function shouldHaveEmptyMetrics()
    {
        $metrics = $this->object->getMetrics();

        $this->assertEquals(0, $metrics->getStatements());
        $this->assertEquals(0, $metrics->getCoveredStatements());
        $this->assertEquals(0, $metrics->getLineCoverage());
    }

    // setServiceName()

    /**
     * @test
     */
    public function setServiceName()
    {
        $expected = 'travis-ci';

        $obj = $this->object->setServiceName($expected);

        $this->assertEquals($expected, $this->object->getServiceName());
        $this->assertSame($obj, $this->object);

        return $this->object;
    }

    // setRepoToken()

    /**
     * @test
     */
    public function setRepoToken()
    {
        $expected = 'token';

        $obj = $this->object->setRepoToken($expected);

        $this->assertEquals($expected, $this->object->getRepoToken());
        $this->assertSame($obj, $this->object);

        return $this->object;
    }

    // setServiceJobId()

    /**
     * @test
     */
    public function setServiceJobId()
    {
        $expected = 'job_id';

        $obj = $this->object->setServiceJobId($expected);

        $this->assertEquals($expected, $this->object->getServiceJobId());
        $this->assertSame($obj, $this->object);

        return $this->object;
    }

    // setGit()

    /**
     * @test
     */
    public function setGit()
    {
        $remotes = array(new Remote());
        $head    = new Commit();
        $git     = new Git('master', $head, $remotes);

        $obj = $this->object->setGit($git);

        $this->assertSame($git, $this->object->getGit());
        $this->assertSame($obj, $this->object);

        return $this->object;
    }

    // setRunAt()

    /**
     * @test
     */
    public function setRunAt()
    {
        $expected = '2013-04-04 11:22:33 +0900';

        $obj = $this->object->setRunAt($expected);

        $this->assertEquals($expected, $this->object->getRunAt());
        $this->assertSame($obj, $this->object);

        return $this->object;
    }



    // addSourceFile()
    // sortSourceFiles()

    /**
     * @test
     */
    public function addSourceFile()
    {
        $sourceFile = $this->createSourceFile();

        $this->object->addSourceFile($sourceFile);
        $this->object->sortSourceFiles();

        $path = $sourceFile->getPath();

        $this->assertTrue($this->object->hasSourceFiles());
        $this->assertSame(array($path => $sourceFile), $this->object->getSourceFiles());
        $this->assertTrue($this->object->hasSourceFile($path));
        $this->assertSame($sourceFile, $this->object->getSourceFile($path));
    }

    // toArray()

    /**
     * @test
     */
    public function toArray()
    {
        $expected = array(
            'source_files' => array(),
        );

        $this->assertEquals($expected, $this->object->toArray());
        $this->assertEquals(json_encode($expected), (string)$this->object);
    }

    /**
     * @test
     */
    public function toArrayWithSourceFiles()
    {
        $sourceFile = $this->createSourceFile();

        $this->object->addSourceFile($sourceFile);

        $expected = array(
            'source_files' => array($sourceFile->toArray()),
        );

        $this->assertEquals($expected, $this->object->toArray());
        $this->assertEquals(json_encode($expected), (string)$this->object);
    }

    // service_name

    /**
     * @test
     * @depends setServiceName
     */
    public function toArrayWithServiceName($object)
    {
        $item = 'travis-ci';

        $expected = array(
            'service_name' => $item,
            'source_files' => array(),
        );

        $this->assertEquals($expected, $object->toArray());
        $this->assertEquals(json_encode($expected), (string)$object);
    }

    // service_job_id

    /**
     * @test
     * @depends setServiceJobId
     */
    public function toArrayWithServiceJobId($object)
    {
        $item = 'job_id';

        $expected = array(
            'service_job_id' => $item,
            'source_files'   => array(),
        );

        $this->assertEquals($expected, $object->toArray());
        $this->assertEquals(json_encode($expected), (string)$object);
    }

    // repo_token

    /**
     * @test
     * @depends setRepoToken
     */
    public function toArrayWithRepoToken($object)
    {
        $item = 'token';

        $expected = array(
            'repo_token'   => $item,
            'source_files' => array(),
        );

        $this->assertEquals($expected, $object->toArray());
        $this->assertEquals(json_encode($expected), (string)$object);
    }

    // git

    /**
     * @test
     * @depends setGit
     */
    public function toArrayWithGit($object)
    {
        $remotes = array(new Remote());
        $head    = new Commit();
        $git     = new Git('master', $head, $remotes);

        $expected = array(
            'git'          => $git->toArray(),
            'source_files' => array(),
        );

        $this->assertSame($expected, $object->toArray());
        $this->assertEquals(json_encode($expected), (string)$object);
    }

    // run_at

    /**
     * @test
     * @depends setRunAt
     */
    public function toArrayWithRunAt($object)
    {
        $item = '2013-04-04 11:22:33 +0900';

        $expected = array(
            'run_at'       => $item,
            'source_files' => array(),
        );

        $this->assertEquals($expected, $object->toArray());
        $this->assertEquals(json_encode($expected), (string)$object);
    }

    // fillJobs()

    /**
     * @test
     */
    public function fillJobsForServiceJobId()
    {
        $serviceName  = 'travis-ci';
        $serviceJobId = '1.1';

        $env = array();
        $env['CI_NAME']   = $serviceName;
        $env['CI_JOB_ID'] = $serviceJobId;

        $object = $this->collectJsonFile();

        $same = $object->fillJobs($env);

        $this->assertSame($same, $object);
        $this->assertEquals($serviceName, $object->getServiceName());
        $this->assertEquals($serviceJobId, $object->getServiceJobId());
    }

    /**
     * @test
     */
    public function fillJobsForServiceNumber()
    {
        $repoToken     = 'token';
        $serviceName   = 'circleci';
        $serviceNumber = '123';

        $env = array();
        $env['COVERALLS_REPO_TOKEN'] = $repoToken;
        $env['CI_NAME']              = $serviceName;
        $env['CI_BUILD_NUMBER']      = $serviceNumber;

        $object = $this->collectJsonFile();

        $same = $object->fillJobs($env);

        $this->assertSame($same, $object);
        $this->assertEquals($repoToken, $object->getRepoToken());
        $this->assertEquals($serviceName, $object->getServiceName());
        $this->assertEquals($serviceNumber, $object->getServiceNumber());
    }

    /**
     * @test
     */
    public function fillJobsForStandardizedEnvVars()
    {
        /*
         * CI_NAME=codeship
         * CI_BUILD_NUMBER=108821
         * CI_BUILD_URL=https://www.codeship.io/projects/2777/builds/108821
         * CI_BRANCH=master
         * CI_PULL_REQUEST=false
         */

        $repoToken          = 'token';
        $serviceName        = 'codeship';
        $serviceNumber      = '108821';
        $serviceBuildUrl    = 'https://www.codeship.io/projects/2777/builds/108821';
        $serviceBranch      = 'master';
        $servicePullRequest = 'false';

        $env = array();
        $env['COVERALLS_REPO_TOKEN'] = $repoToken;
        $env['CI_NAME']              = $serviceName;
        $env['CI_BUILD_NUMBER']      = $serviceNumber;
        $env['CI_BUILD_URL']         = $serviceBuildUrl;
        $env['CI_BRANCH']            = $serviceBranch;
        $env['CI_PULL_REQUEST']      = $servicePullRequest;

        $object = $this->collectJsonFile();

        $same = $object->fillJobs($env);

        $this->assertSame($same, $object);
        $this->assertEquals($repoToken, $object->getRepoToken());
        $this->assertEquals($serviceName, $object->getServiceName());
        $this->assertEquals($serviceNumber, $object->getServiceNumber());
        $this->assertEquals($serviceBuildUrl, $object->getServiceBuildUrl());
        $this->assertEquals($serviceBranch, $object->getServiceBranch());
        $this->assertEquals($servicePullRequest, $object->getServicePullRequest());
    }

    /**
     * @test
     */
    public function fillJobsForServiceEventType()
    {
        $repoToken        = 'token';
        $serviceName      = 'php-coveralls';
        $serviceEventType = 'manual';

        $env = array();
        $env['COVERALLS_REPO_TOKEN']  = $repoToken;
        $env['COVERALLS_RUN_LOCALLY'] = '1';
        $env['COVERALLS_EVENT_TYPE']  = $serviceEventType;
        $env['CI_NAME']               = $serviceName;

        $object = $this->collectJsonFile();

        $same = $object->fillJobs($env);

        $this->assertSame($same, $object);
        $this->assertEquals($repoToken, $object->getRepoToken());
        $this->assertEquals($serviceName, $object->getServiceName());
        $this->assertNull($object->getServiceJobId());
        $this->assertEquals($serviceEventType, $object->getServiceEventType());
    }

    /**
     * @test
     */
    public function fillJobsForUnsupportedJob()
    {
        $repoToken = 'token';

        $env = array();
        $env['COVERALLS_REPO_TOKEN'] = $repoToken;

        $object = $this->collectJsonFile();

        $same = $object->fillJobs($env);

        $this->assertSame($same, $object);
        $this->assertEquals($repoToken, $object->getRepoToken());
    }

    /**
     * @test
     * @expectedException RuntimeException
     */
    public function throwRuntimeExceptionOnFillingJobsIfInvalidEnv()
    {
        $env = array();

        $object = $this->collectJsonFile();

        $object->fillJobs($env);
    }

    /**
     * @test
     * @expectedException RuntimeException
     */
    public function throwRuntimeExceptionOnFillingJobsIfNoSourceFiles()
    {
        $env = array();
        $env['TRAVIS']        = true;
        $env['TRAVIS_JOB_ID'] = '1.1';

        $object = $this->collectJsonFileWithoutSourceFiles();

        $object->fillJobs($env);
    }

    // reportLineCoverage()

    /**
     * @test
     */
    public function reportLineCoverage()
    {
        $object = $this->collectJsonFile();

        $this->assertEquals(50, $object->reportLineCoverage());

        $metrics = $object->getMetrics();

        $this->assertEquals(2, $metrics->getStatements());
        $this->assertEquals(1, $metrics->getCoveredStatements());
        $this->assertEquals(50, $metrics->getLineCoverage());
    }

    // excludeNoStatementsFiles()

    /**
     * @test
     */
    public function excludeNoStatementsFiles()
    {
        $rootDir = $this->rootDir . DIRECTORY_SEPARATOR;

        $object = $this->collectJsonFile();

        // before excluding
        $sourceFiles = $object->getSourceFiles();
        $this->assertCount(4, $sourceFiles);

        // filenames
        $paths     = array_keys($sourceFiles);
        $filenames = array_map(function ($path) use ($rootDir) {return str_replace($rootDir, '', $path);}, $paths);

        $this->assertContains('test.php', $filenames);
        $this->assertContains('test2.php', $filenames);
        $this->assertContains('TestInterface.php', $filenames);
        $this->assertContains('AbstractClass.php', $filenames);

        // after excluding
        $object->excludeNoStatementsFiles();

        $sourceFiles = $object->getSourceFiles();
        $this->assertCount(2, $sourceFiles);

        // filenames
        $paths     = array_keys($sourceFiles);
        $filenames = array_map(function ($path) use ($rootDir) {return str_replace($rootDir, '', $path);}, $paths);

        $this->assertContains('test.php', $filenames);
        $this->assertContains('test2.php', $filenames);
        $this->assertNotContains('TestInterface.php', $filenames);
        $this->assertNotContains('AbstractClass.php', $filenames);
    }
}
