<?php

namespace Guzzle\Tests\Service\Command\LocationVisitor\Response;

use Guzzle\Service\Description\Parameter;
use Guzzle\Http\Message\Response;
use Guzzle\Service\Command\LocationVisitor\Response\BodyVisitor as Visitor;

/**
 * @covers Guzzle\Service\Command\LocationVisitor\Response\BodyVisitor
 */
class BodyVisitorTest extends AbstractResponseVisitorTest
{
    public function testVisitsLocation()
    {
        $visitor = new Visitor();
        $param = new Parameter(array('location' => 'body', 'name' => 'foo'));
        $visitor->visit($this->command, $this->response, $param, $this->value);
        $this->assertEquals('Foo', (string) $this->value['foo']);
    }
}
