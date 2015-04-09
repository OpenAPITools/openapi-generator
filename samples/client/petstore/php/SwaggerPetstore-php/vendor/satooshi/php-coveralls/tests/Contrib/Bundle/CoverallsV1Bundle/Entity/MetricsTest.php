<?php
namespace Contrib\Bundle\CoverallsV1Bundle\Entity;

/**
 * @covers Contrib\Bundle\CoverallsV1Bundle\Entity\Metrics
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class MetricsTest extends \PHPUnit_Framework_TestCase
{
    protected function setUp()
    {
        $this->coverage = array_fill(0, 5, null);
        $this->coverage[1] = 1;
        $this->coverage[3] = 1;
        $this->coverage[4] = 0;
    }

    // hasStatements()
    // getStatements()

    /**
     * @test
     */
    public function shouldNotHaveStatementsOnConstructionWithoutCoverage()
    {
        $object = new Metrics();

        $this->assertFalse($object->hasStatements());
        $this->assertEquals(0, $object->getStatements());
    }

    /**
     * @test
     */
    public function shouldHaveStatementsOnConstructionWithCoverage()
    {
        $object = new Metrics($this->coverage);

        $this->assertTrue($object->hasStatements());
        $this->assertEquals(3, $object->getStatements());
    }

    // getCoveredStatements()

    /**
     * @test
     */
    public function shouldNotHaveCoveredStatementsOnConstructionWithoutCoverage()
    {
        $object = new Metrics();

        $this->assertEquals(0, $object->getCoveredStatements());
    }

    /**
     * @test
     */
    public function shouldHaveCoveredStatementsOnConstructionWithCoverage()
    {
        $object = new Metrics($this->coverage);

        $this->assertEquals(2, $object->getCoveredStatements());
    }

    // getLineCoverage()

    /**
     * @test
     */
    public function shouldNotHaveLineCoverageOnConstructionWithoutCoverage()
    {
        $object = new Metrics();

        $this->assertEquals(0, $object->getLineCoverage());
    }

    /**
     * @test
     */
    public function shouldHaveLineCoverageOnConstructionWithCoverage()
    {
        $object = new Metrics($this->coverage);

        $this->assertEquals(200/3, $object->getLineCoverage());
    }

    // merge()

    /**
     * @test
     */
    public function mergeThatWithEmptyMetrics()
    {
        $object = new Metrics();
        $that   = new Metrics($this->coverage);
        $object->merge($that);

        $this->assertEquals(3, $object->getStatements());
        $this->assertEquals(2, $object->getCoveredStatements());
        $this->assertEquals(200/3, $object->getLineCoverage());
    }

    /**
     * @test
     */
    public function mergeThat()
    {
        $object = new Metrics($this->coverage);
        $that   = new Metrics($this->coverage);
        $object->merge($that);

        $this->assertEquals(6, $object->getStatements());
        $this->assertEquals(4, $object->getCoveredStatements());
        $this->assertEquals(400/6, $object->getLineCoverage());
    }
}
