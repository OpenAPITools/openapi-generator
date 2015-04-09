<?php

namespace Guzzle\Tests\Service\Command\LocationVisitor\Request;

use Guzzle\Service\Description\Parameter;
use Guzzle\Service\Command\LocationVisitor\Request\HeaderVisitor as Visitor;

/**
 * @covers Guzzle\Service\Command\LocationVisitor\Request\HeaderVisitor
 */
class HeaderVisitorTest extends AbstractVisitorTestCase
{
    /**
     * @expectedException \Guzzle\Common\Exception\InvalidArgumentException
     */
    public function testValidatesHeaderMapsAreArrays()
    {
        $visitor = new Visitor();
        $param = $this->getNestedCommand('header')->getParam('foo')->setSentAs('test');
        $param->setAdditionalProperties(new Parameter(array()));
        $visitor->visit($this->command, $this->request, $param, 'test');
    }

    public function testVisitsLocation()
    {
        $visitor = new Visitor();
        $param = $this->getNestedCommand('header')->getParam('foo')->setSentAs('test');
        $param->setAdditionalProperties(false);
        $visitor->visit($this->command, $this->request, $param, '123');
        $this->assertEquals('123', (string) $this->request->getHeader('test'));
    }

    public function testVisitsMappedPrefixHeaders()
    {
        $visitor = new Visitor();
        $param = $this->getNestedCommand('header')->getParam('foo')->setSentAs('test');
        $param->setSentAs('x-foo-');
        $param->setAdditionalProperties(new Parameter(array(
            'type' => 'string'
        )));
        $visitor->visit($this->command, $this->request, $param, array(
            'bar' => 'test',
            'baz' => '123'
        ));
        $this->assertEquals('test', (string) $this->request->getHeader('x-foo-bar'));
        $this->assertEquals('123', (string) $this->request->getHeader('x-foo-baz'));
    }
}
