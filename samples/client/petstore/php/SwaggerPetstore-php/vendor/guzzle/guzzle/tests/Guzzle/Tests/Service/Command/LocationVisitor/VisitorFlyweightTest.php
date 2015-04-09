<?php

namespace Guzzle\Tests\Service\Command;

use Guzzle\Service\Command\LocationVisitor\VisitorFlyweight;
use Guzzle\Service\Command\LocationVisitor\Request\JsonVisitor as JsonRequestVisitor;
use Guzzle\Service\Command\LocationVisitor\Response\JsonVisitor as JsonResponseVisitor;

/**
 * @covers Guzzle\Service\Command\LocationVisitor\VisitorFlyweight
 */
class VisitorFlyweightTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testUsesDefaultMappingsWithGetInstance()
    {
        $f = VisitorFlyweight::getInstance();
        $this->assertInstanceOf('Guzzle\Service\Command\LocationVisitor\Request\JsonVisitor', $f->getRequestVisitor('json'));
        $this->assertInstanceOf('Guzzle\Service\Command\LocationVisitor\Response\JsonVisitor', $f->getResponseVisitor('json'));
    }

    public function testCanUseCustomMappings()
    {
        $f = new VisitorFlyweight(array());
        $this->assertEquals(array(), $this->readAttribute($f, 'mappings'));
    }

    /**
     * @expectedException \InvalidArgumentException
     * @expectedExceptionMessage No request visitor has been mapped for foo
     */
    public function testThrowsExceptionWhenRetrievingUnknownVisitor()
    {
        VisitorFlyweight::getInstance()->getRequestVisitor('foo');
    }

    public function testCachesVisitors()
    {
        $f = new VisitorFlyweight();
        $v1 = $f->getRequestVisitor('json');
        $this->assertSame($v1, $f->getRequestVisitor('json'));
    }

    public function testAllowsAddingVisitors()
    {
        $f = new VisitorFlyweight();
        $j1 = new JsonRequestVisitor();
        $j2 = new JsonResponseVisitor();
        $f->addRequestVisitor('json', $j1);
        $f->addResponseVisitor('json', $j2);
        $this->assertSame($j1, $f->getRequestVisitor('json'));
        $this->assertSame($j2, $f->getResponseVisitor('json'));
    }
}
