<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Command;

use Symfony\Component\Console\Tester\CommandTester;
use Symfony\Component\Console\Application;

/**
 * @covers Contrib\Bundle\CoverallsV1Bundle\Command\CoverallsV1JobsCommand
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class CoverallsV1JobsCommandTest extends \PHPUnit_Framework_TestCase
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

        $command = new CoverallsV1JobsCommand();
        $command->setRootDir($this->rootDir);

        $app = new Application();
        $app->add($command);

        $command = $app->find('coveralls:v1:jobs');
        $commandTester = new CommandTester($command);

        $_SERVER['TRAVIS']        = true;
        $_SERVER['TRAVIS_JOB_ID'] = 'command_test';

        $actual = $commandTester->execute(
            array(
                'command'   => $command->getName(),
                '--dry-run' => true,
                '--config'  => 'coveralls.yml',
                '--env'     => 'test',
            )
        );

        $this->assertEquals(0, $actual);
    }
}
