<?php

namespace Guzzle\Tests\Service\Command;

use Guzzle\Http\Message\EntityEnclosingRequest;
use Guzzle\Http\Message\Response;
use Guzzle\Service\Client;
use Guzzle\Service\Command\OperationCommand;
use Guzzle\Service\Description\Operation;
use Guzzle\Service\Description\ServiceDescription;
use Guzzle\Service\Command\DefaultRequestSerializer;
use Guzzle\Service\Resource\Model;
use Guzzle\Service\Command\LocationVisitor\VisitorFlyweight;

/**
 * @covers Guzzle\Service\Command\OperationCommand
 */
class OperationCommandTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testHasRequestSerializer()
    {
        $operation = new OperationCommand();
        $a = $operation->getRequestSerializer();
        $b = new DefaultRequestSerializer(VisitorFlyweight::getInstance());
        $operation->setRequestSerializer($b);
        $this->assertNotSame($a, $operation->getRequestSerializer());
    }

    public function testPreparesRequestUsingSerializer()
    {
        $op = new OperationCommand(array(), new Operation());
        $op->setClient(new Client());
        $s = $this->getMockBuilder('Guzzle\Service\Command\RequestSerializerInterface')
            ->setMethods(array('prepare'))
            ->getMockForAbstractClass();
        $s->expects($this->once())
            ->method('prepare')
            ->will($this->returnValue(new EntityEnclosingRequest('POST', 'http://foo.com')));
        $op->setRequestSerializer($s);
        $op->prepare();
    }

    public function testParsesResponsesWithResponseParser()
    {
        $op = new OperationCommand(array(), new Operation());
        $p = $this->getMockBuilder('Guzzle\Service\Command\ResponseParserInterface')
            ->setMethods(array('parse'))
            ->getMockForAbstractClass();
        $p->expects($this->once())
            ->method('parse')
            ->will($this->returnValue(array('foo' => 'bar')));
        $op->setResponseParser($p);
        $op->setClient(new Client());
        $request = $op->prepare();
        $request->setResponse(new Response(200), true);
        $this->assertEquals(array('foo' => 'bar'), $op->execute());
    }

    public function testParsesResponsesUsingModelParserWhenMatchingModelIsFound()
    {
        $description = ServiceDescription::factory(array(
            'operations' => array(
                'foo' => array('responseClass' => 'bar', 'responseType' => 'model')
            ),
            'models' => array(
                'bar' => array(
                    'type' => 'object',
                    'properties' => array(
                        'Baz' => array('type' => 'string', 'location' => 'xml')
                    )
                )
            )
        ));

        $op = new OperationCommand(array(), $description->getOperation('foo'));
        $op->setClient(new Client());
        $request = $op->prepare();
        $request->setResponse(new Response(200, array(
            'Content-Type' => 'application/xml'
        ), '<Foo><Baz>Bar</Baz></Foo>'), true);
        $result = $op->execute();
        $this->assertEquals(new Model(array('Baz' => 'Bar')), $result);
    }

    public function testAllowsRawResponses()
    {
        $description = new ServiceDescription(array(
            'operations' => array('foo' => array('responseClass' => 'bar', 'responseType' => 'model')),
            'models'     => array('bar' => array())
        ));
        $op = new OperationCommand(array(
            OperationCommand::RESPONSE_PROCESSING => OperationCommand::TYPE_RAW
        ), $description->getOperation('foo'));
        $op->setClient(new Client());
        $request = $op->prepare();
        $response = new Response(200, array(
            'Content-Type' => 'application/xml'
        ), '<Foo><Baz>Bar</Baz></Foo>');
        $request->setResponse($response, true);
        $this->assertSame($response, $op->execute());
    }
}
