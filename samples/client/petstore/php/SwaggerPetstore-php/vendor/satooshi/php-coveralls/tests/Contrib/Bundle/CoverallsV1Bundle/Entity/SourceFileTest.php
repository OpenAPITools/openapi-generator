<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Entity;

/**
 * @covers Contrib\Bundle\CoverallsV1Bundle\Entity\SourceFile
 * @covers Contrib\Bundle\CoverallsV1Bundle\Entity\Coveralls
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class SourceFileTest extends \PHPUnit_Framework_TestCase
{
    protected function setUp()
    {
        $this->dir      = realpath(__DIR__ . '/../../../../');
        $this->rootDir  = realpath($this->dir . '/prj/files');
        $this->filename = 'test.php';
        $this->path     = $this->rootDir . DIRECTORY_SEPARATOR . $this->filename;

        $this->object = new SourceFile($this->path, $this->filename);
    }

    // getName()

    /**
     * @test
     */
    public function shouldHaveNameOnConstruction()
    {
        $this->assertEquals($this->filename, $this->object->getName());
    }

    // getSource()

    /**
     * @test
     */
    public function shouldHaveSourceOnConstruction()
    {
        $expected = trim(file_get_contents($this->path));

        $this->assertEquals($expected, $this->object->getSource());
    }

    // getCoverage()

    /**
     * @test
     */
    public function shouldHaveNullCoverageOnConstruction()
    {
        $expected = array_fill(0, 9, null);

        $this->assertEquals($expected, $this->object->getCoverage());
    }

    // getPath()

    /**
     * @test
     */
    public function shouldHavePathOnConstruction()
    {
        $this->assertEquals($this->path, $this->object->getPath());
    }

    // getFileLines()

    /**
     * @test
     */
    public function shouldHaveFileLinesOnConstruction()
    {
        $this->assertEquals(9, $this->object->getFileLines());
    }

    // toArray()

    /**
     * @test
     */
    public function toArray()
    {
        $expected = array(
            'name'     => $this->filename,
            'source'   => trim(file_get_contents($this->path)),
            'coverage' => array_fill(0, 9, null),
        );

        $this->assertEquals($expected, $this->object->toArray());
        $this->assertEquals(json_encode($expected), (string)$this->object);
    }

    // addCoverage()

    /**
     * @test
     */
    public function addCoverage()
    {
        $this->object->addCoverage(5, 1);

        $expected = array_fill(0, 9, null);
        $expected[5] = 1;

        $this->assertEquals($expected, $this->object->getCoverage());
    }

    // getMetrics()
    // reportLineCoverage()

    /**
     * @test
     */
    public function shouldLineCoverageZeroWithoutAddingCoverage()
    {
        $metrics = $this->object->getMetrics();

        $this->assertEquals(0, $metrics->getStatements());
        $this->assertEquals(0, $metrics->getCoveredStatements());
        $this->assertEquals(0, $metrics->getLineCoverage());
        $this->assertEquals(0, $this->object->reportLineCoverage());
    }

    /**
     * @test
     */
    public function shouldLineCoverageAfterAddingCoverage()
    {
        $this->object->addCoverage(6, 1);

        $metrics = $this->object->getMetrics();

        $this->assertEquals(1, $metrics->getStatements());
        $this->assertEquals(1, $metrics->getCoveredStatements());
        $this->assertEquals(100, $metrics->getLineCoverage());
        $this->assertEquals(100, $this->object->reportLineCoverage());
    }
}
