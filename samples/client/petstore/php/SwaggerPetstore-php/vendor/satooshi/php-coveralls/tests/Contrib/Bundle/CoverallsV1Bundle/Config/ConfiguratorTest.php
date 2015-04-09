<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Config;

/**
 * @covers Contrib\Bundle\CoverallsV1Bundle\Config\Configurator
 * @covers Contrib\Bundle\CoverallsV1Bundle\Config\CoverallsConfiguration
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class ConfiguratorTest extends \PHPUnit_Framework_TestCase
{
    protected function setUp()
    {
        $this->rootDir        = realpath(__DIR__ . '/../../../../prj');
        $this->srcDir         = $this->rootDir . '/src';
        $this->buildDir       = $this->rootDir . '/build';
        $this->logsDir        = $this->rootDir . '/build/logs';
        $this->cloverXmlPath  = $this->logsDir . '/clover.xml';
        $this->cloverXmlPath1 = $this->logsDir . '/clover-part1.xml';
        $this->cloverXmlPath2 = $this->logsDir . '/clover-part2.xml';
        $this->jsonPath       = $this->logsDir . DIRECTORY_SEPARATOR . 'coveralls-upload.json';

        $this->object = new Configurator();
    }

    protected function tearDown()
    {
        $this->rmFile($this->cloverXmlPath);
        $this->rmFile($this->cloverXmlPath1);
        $this->rmFile($this->cloverXmlPath2);
        $this->rmFile($this->jsonPath);
        $this->rmDir($this->srcDir);
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

    protected function makeProjectDir($srcDir, $logsDir, $cloverXmlPaths, $logsDirUnwritable = false, $jsonPathUnwritable = false)
    {
        if ($srcDir !== null) {
            mkdir($srcDir, 0777, true);
        }

        if ($logsDir !== null) {
            mkdir($logsDir, 0777, true);
        }

        if ($cloverXmlPaths !== null) {
            if (is_array($cloverXmlPaths)) {
                foreach ($cloverXmlPaths as $cloverXmlPath) {
                    touch($cloverXmlPath);
                }
            } else {
                touch($cloverXmlPaths);
            }
        }

        if ($logsDirUnwritable) {
            chmod($logsDir, 0577);
        }

        if ($jsonPathUnwritable) {
            touch($this->jsonPath);
            chmod($this->jsonPath, 0577);
        }
    }

    // custom assertion

    protected function assertConfiguration(Configuration $config, $srcDir, array $cloverXml, $jsonPath, $excludeNoStatements = false)
    {
        $this->assertEquals($srcDir, $config->getSrcDir());
        $this->assertEquals($cloverXml, $config->getCloverXmlPaths());
        $this->assertEquals($jsonPath, $config->getJsonPath());
        $this->assertEquals($excludeNoStatements, $config->isExcludeNoStatements());
    }

    // load()

    /**
     * @test
     */
    public function shouldLoadNonExistingYml()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath);

        $path = realpath(__DIR__ . '/yaml/dummy.yml');

        $config = $this->object->load($path, $this->rootDir);

        $this->assertConfiguration($config, $this->srcDir, array($this->cloverXmlPath), $this->jsonPath);
    }

    // default src_dir not found

    /**
     * @test
     * @expectedException Symfony\Component\Config\Definition\Exception\InvalidConfigurationException
     */
    public function throwInvalidConfigurationExceptionOnLoadEmptyYmlIfSrcDirNotFound()
    {
        $this->makeProjectDir(null, $this->logsDir, $this->cloverXmlPath);

        $path = realpath(__DIR__ . '/yaml/dummy.yml');

        $this->object->load($path, $this->rootDir);
    }

    // default coverage_clover not found

    /**
     * @test
     * @expectedException Symfony\Component\Config\Definition\Exception\InvalidConfigurationException
     */
    public function throwInvalidConfigurationExceptionOnLoadEmptyYmlIfCoverageCloverNotFound()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, null);

        $path = realpath(__DIR__ . '/yaml/dummy.yml');

        $this->object->load($path, $this->rootDir);
    }

    // default json_path not writable

    /**
     * @test
     * @expectedException Symfony\Component\Config\Definition\Exception\InvalidConfigurationException
     */
    public function throwInvalidConfigurationExceptionOnLoadEmptyYmlIfJsonPathDirNotWritable()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath, true);

        $path = realpath(__DIR__ . '/yaml/dummy.yml');

        $this->object->load($path, $this->rootDir);
    }

    /**
     * @test
     * @expectedException Symfony\Component\Config\Definition\Exception\InvalidConfigurationException
     */
    public function throwInvalidConfigurationExceptionOnLoadEmptyYmlIfJsonPathNotWritable()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath, false, true);

        $path = realpath(__DIR__ . '/yaml/dummy.yml');

        $this->object->load($path, $this->rootDir);
    }

    // no configuration

    /**
     * @test
     */
    public function shouldLoadEmptyYml()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath);

        $path = realpath(__DIR__ . '/yaml/empty.yml');

        $config = $this->object->load($path, $this->rootDir);

        $this->assertConfiguration($config, $this->srcDir, array($this->cloverXmlPath), $this->jsonPath);
    }

    // load default value yml

    /**
     * @test
     */
    public function shouldLoadSrcDirYmlContainingDefaultValue()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath);

        $path = realpath(__DIR__ . '/yaml/src_dir.yml');

        $config = $this->object->load($path, $this->rootDir);

        $this->assertConfiguration($config, $this->srcDir, array($this->cloverXmlPath), $this->jsonPath);
    }

    /**
     * @test
     */
    public function shouldLoadCoverageCloverYmlContainingDefaultValue()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath);

        $path = realpath(__DIR__ . '/yaml/coverage_clover.yml');

        $config = $this->object->load($path, $this->rootDir);

        $this->assertConfiguration($config, $this->srcDir, array($this->cloverXmlPath), $this->jsonPath);
    }

    /**
     * @test
     */
    public function shouldLoadCoverageCloverYmlContainingGlobValue()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, array($this->cloverXmlPath1, $this->cloverXmlPath2));

        $path = realpath(__DIR__ . '/yaml/coverage_clover_glob.yml');

        $config = $this->object->load($path, $this->rootDir);

        $this->assertConfiguration($config, $this->srcDir, array($this->cloverXmlPath1, $this->cloverXmlPath2), $this->jsonPath);
    }

    /**
     * @test
     */
    public function shouldLoadCoverageCloverYmlContainingArrayValue()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, array($this->cloverXmlPath1, $this->cloverXmlPath2));

        $path = realpath(__DIR__ . '/yaml/coverage_clover_array.yml');

        $config = $this->object->load($path, $this->rootDir);

        $this->assertConfiguration($config, $this->srcDir, array($this->cloverXmlPath1, $this->cloverXmlPath2), $this->jsonPath);
    }

    /**
     * @test
     */
    public function shouldLoadJsonPathYmlContainingDefaultValue()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath);

        $path = realpath(__DIR__ . '/yaml/json_path.yml');

        $config = $this->object->load($path, $this->rootDir);

        $this->assertConfiguration($config, $this->srcDir, array($this->cloverXmlPath), $this->jsonPath);
    }

    /**
     * @test
     */
    public function shouldLoadExcludeNoStmtYmlContainingTrue()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath);

        $path = realpath(__DIR__ . '/yaml/exclude_no_stmt_true.yml');

        $config = $this->object->load($path, $this->rootDir);

        $this->assertConfiguration($config, $this->srcDir, array($this->cloverXmlPath), $this->jsonPath, true);
    }

    /**
     * @test
     */
    public function shouldLoadExcludeNoStmtYmlContainingFalse()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath);

        $path = realpath(__DIR__ . '/yaml/exclude_no_stmt_false.yml');

        $config = $this->object->load($path, $this->rootDir);

        $this->assertConfiguration($config, $this->srcDir, array($this->cloverXmlPath), $this->jsonPath, false);
    }

    // configured src_dir not found

    /**
     * @test
     * @expectedException Symfony\Component\Config\Definition\Exception\InvalidConfigurationException
     */
    public function throwInvalidConfigurationExceptionOnLoadSrcDirYmlIfSrcDirNotFound()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath);

        $path = realpath(__DIR__ . '/yaml/src_dir_not_found.yml');

        $this->object->load($path, $this->rootDir);
    }

    // configured coverage_clover not found

    /**
     * @test
     * @expectedException Symfony\Component\Config\Definition\Exception\InvalidConfigurationException
     */
    public function throwInvalidConfigurationExceptionOnLoadCoverageCloverYmlIfCoverageCloverNotFound()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath);

        $path = realpath(__DIR__ . '/yaml/coverage_clover_not_found.yml');

        $this->object->load($path, $this->rootDir);
    }

    /**
     * @test
     * @expectedException Symfony\Component\Config\Definition\Exception\InvalidConfigurationException
     */
    public function throwInvalidConfigurationExceptionOnLoadCoverageCloverYmlIfCoverageCloverIsNotString()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath);

        $path = realpath(__DIR__ . '/yaml/coverage_clover_invalid.yml');

        $this->object->load($path, $this->rootDir);
    }

    // configured json_path not found

    /**
     * @test
     * @expectedException Symfony\Component\Config\Definition\Exception\InvalidConfigurationException
     */
    public function throwInvalidConfigurationExceptionOnLoadJsonPathYmlIfJsonPathNotFound()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath);

        $path = realpath(__DIR__ . '/yaml/json_path_not_found.yml');

        $this->object->load($path, $this->rootDir);
    }

    // configured exclude_no_stmt invalid

    /**
     * @test
     * @expectedException Symfony\Component\Config\Definition\Exception\InvalidConfigurationException
     */
    public function throwInvalidConfigurationExceptionOnLoadExcludeNoStmtYmlIfInvalid()
    {
        $this->makeProjectDir($this->srcDir, $this->logsDir, $this->cloverXmlPath);

        $path = realpath(__DIR__ . '/yaml/exclude_no_stmt_invalid.yml');

        $this->object->load($path, $this->rootDir);
    }
}
