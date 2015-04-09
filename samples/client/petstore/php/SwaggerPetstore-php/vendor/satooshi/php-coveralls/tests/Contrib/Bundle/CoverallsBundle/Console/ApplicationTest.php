<?php
namespace Contrib\Bundle\CoverallsBundle\Console;

use Symfony\Component\Console\Tester\ApplicationTester;

/**
 * @covers Contrib\Bundle\CoverallsBundle\Console\Application
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class ApplicationTest extends \PHPUnit_Framework_TestCase
{
    protected function setUp()
    {
        $this->rootDir       = realpath(__DIR__ . '/../../../../prj');
        $this->srcDir        = $this->rootDir . '/files';
        $this->buildDir      = $this->rootDir . '/build';
        $this->logsDir       = $this->rootDir . '/build/logs';
        $this->cloverXmlPath = $this->logsDir . '/clover.xml';
        $this->jsonPath      = $this->logsDir . DIRECTORY_SEPARATOR . 'coveralls-upload.json';
    }

    protected function tearDown()
    {
        $this->rmFile($this->cloverXmlPath);
        $this->rmFile($this->jsonPath);
        $this->rmDir($this->logsDir);
        $this->rmDir($this->buildDir);
    }

    protected function rmFile($file)
    {
        if (is_file($file)) {
            chmod(dirname($file), 0777);
            unlink($file);
        }
    }

    protected function rmDir($dir)
    {
        if (is_dir($dir)) {
            chmod($dir, 0777);
            rmdir($dir);
        }
    }

    protected function makeProjectDir($logsDir, $cloverXmlPath, $logsDirUnwritable = false, $jsonPathUnwritable = false)
    {
        if ($logsDir !== null) {
            mkdir($logsDir, 0777, true);
        }

        if ($cloverXmlPath !== null) {
            file_put_contents($cloverXmlPath, $this->getCloverXml());
        }

        if ($logsDirUnwritable) {
            chmod($logsDir, 0577);
        }

        if ($jsonPathUnwritable) {
            touch($this->jsonPath);
            chmod($this->jsonPath, 0577);
        }
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
        return sprintf($xml, $this->srcDir, $this->srcDir);
    }

    /**
     * @test
     */
    public function shouldExecuteCoverallsV1JobsCommand()
    {
        $this->makeProjectDir($this->logsDir, $this->cloverXmlPath);

        $app = new Application($this->rootDir, 'Coveralls API client for PHP', '1.0.0');
        $app->setAutoExit(false); // avoid to call exit() in Application

        // run
        $_SERVER['TRAVIS']        = true;
        $_SERVER['TRAVIS_JOB_ID'] = 'application_test';

        $tester = new ApplicationTester($app);
        $actual = $tester->run(
            array(
                '--dry-run' => true,
                '--config'  => 'coveralls.yml',
            )
        );

        $this->assertEquals(0, $actual);
    }
}
