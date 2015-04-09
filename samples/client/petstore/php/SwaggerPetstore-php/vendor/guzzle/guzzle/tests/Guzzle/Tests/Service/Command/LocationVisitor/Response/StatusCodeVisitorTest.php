<?php

namespace Guzzle\Tests\Service\Command\LocationVisitor\Response;

use Guzzle\Service\Description\Parameter;
use Guzzle\Http\Message\Response;
use Guzzle\Service\Command\LocationVisitor\Response\StatusCodeVisitor as Visitor;

/**
 * @covers Guzzle\Service\Command\LocationVisitor\Response\StatusCodeVisitor
 */
class StatusCodeVisitorTest extends AbstractResponseVisitorTest
{
    public function testVisitsLocation()
    {
        $visitor = new Visitor();
        $param = new Parameter(array('location' => 'statusCode', 'name' => 'code'));
        $visitor->visit($this->command, $this->response, $param, $this->value);
        $this->assertEquals(200, $this->value['code']);
    }
}
