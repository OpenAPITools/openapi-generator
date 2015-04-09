<?php

namespace Guzzle\Tests\Service\Description;

use Guzzle\Service\Description\Operation;
use Guzzle\Service\Description\Parameter;
use Guzzle\Service\Description\ServiceDescription;

/**
 * @covers Guzzle\Service\Description\Operation
 */
class OperationTest extends \Guzzle\Tests\GuzzleTestCase
{
    public static function strtoupper($string)
    {
        return strtoupper($string);
    }

    public function testOperationIsDataObject()
    {
        $c = new Operation(array(
            'name'               => 'test',
            'summary'            => 'doc',
            'notes'              => 'notes',
            'documentationUrl'   => 'http://www.example.com',
            'httpMethod'         => 'POST',
            'uri'                => '/api/v1',
            'responseClass'      => 'array',
            'responseNotes'      => 'returns the json_decoded response',
            'deprecated'         => true,
            'parameters'         => array(
                'key' => array(
                    'required'  => true,
                    'type'      => 'string',
                    'maxLength' => 10
                ),
                'key_2' => array(
                    'required' => true,
                    'type'     => 'integer',
                    'default'  => 10
                )
            )
        ));

        $this->assertEquals('test', $c->getName());
        $this->assertEquals('doc', $c->getSummary());
        $this->assertEquals('http://www.example.com', $c->getDocumentationUrl());
        $this->assertEquals('POST', $c->getHttpMethod());
        $this->assertEquals('/api/v1', $c->getUri());
        $this->assertEquals('array', $c->getResponseClass());
        $this->assertEquals('returns the json_decoded response', $c->getResponseNotes());
        $this->assertTrue($c->getDeprecated());
        $this->assertEquals('Guzzle\\Service\\Command\\OperationCommand', $c->getClass());
        $this->assertEquals(array(
            'key' => new Parameter(array(
                'name'      => 'key',
                'required'  => true,
                'type'      => 'string',
                'maxLength' => 10,
                'parent'    => $c
            )),
            'key_2' => new Parameter(array(
                'name'     => 'key_2',
                'required' => true,
                'type'     => 'integer',
                'default'  => 10,
                'parent'   => $c
            ))
        ), $c->getParams());

        $this->assertEquals(new Parameter(array(
            'name'     => 'key_2',
            'required' => true,
            'type'     => 'integer',
            'default'  => 10,
            'parent'   => $c
        )), $c->getParam('key_2'));

        $this->assertNull($c->getParam('afefwef'));
        $this->assertArrayNotHasKey('parent', $c->getParam('key_2')->toArray());
    }

    public function testAllowsConcreteCommands()
    {
        $c = new Operation(array(
            'name' => 'test',
            'class' => 'Guzzle\\Service\\Command\ClosureCommand',
            'parameters' => array(
                'p' => new Parameter(array(
                    'name' => 'foo'
                ))
            )
        ));
        $this->assertEquals('Guzzle\\Service\\Command\ClosureCommand', $c->getClass());
    }

    public function testConvertsToArray()
    {
        $data = array(
            'name'             => 'test',
            'class'            => 'Guzzle\\Service\\Command\ClosureCommand',
            'summary'          => 'test',
            'documentationUrl' => 'http://www.example.com',
            'httpMethod'       => 'PUT',
            'uri'              => '/',
            'parameters'       => array('p' => array('name' => 'foo'))
        );
        $c = new Operation($data);
        $toArray = $c->toArray();
        unset($data['name']);
        $this->assertArrayHasKey('parameters', $toArray);
        $this->assertInternalType('array', $toArray['parameters']);

        // Normalize the array
        unset($data['parameters']);
        unset($toArray['parameters']);

        $data['responseType'] = 'primitive';
        $data['responseClass'] = 'array';
        $this->assertEquals($data, $toArray);
    }

    public function testDeterminesIfHasParam()
    {
        $command = $this->getTestCommand();
        $this->assertTrue($command->hasParam('data'));
        $this->assertFalse($command->hasParam('baz'));
    }

    public function testReturnsParamNames()
    {
        $command = $this->getTestCommand();
        $this->assertEquals(array('data'), $command->getParamNames());
    }

    protected function getTestCommand()
    {
        return new Operation(array(
            'parameters' => array(
                'data' => new Parameter(array(
                    'type' => 'string'
                ))
            )
        ));
    }

    public function testCanBuildUpCommands()
    {
        $c = new Operation(array());
        $c->setName('foo')
            ->setClass('Baz')
            ->setDeprecated(false)
            ->setSummary('summary')
            ->setDocumentationUrl('http://www.foo.com')
            ->setHttpMethod('PUT')
            ->setResponseNotes('oh')
            ->setResponseClass('string')
            ->setUri('/foo/bar')
            ->addParam(new Parameter(array(
                'name' => 'test'
            )));

        $this->assertEquals('foo', $c->getName());
        $this->assertEquals('Baz', $c->getClass());
        $this->assertEquals(false, $c->getDeprecated());
        $this->assertEquals('summary', $c->getSummary());
        $this->assertEquals('http://www.foo.com', $c->getDocumentationUrl());
        $this->assertEquals('PUT', $c->getHttpMethod());
        $this->assertEquals('oh', $c->getResponseNotes());
        $this->assertEquals('string', $c->getResponseClass());
        $this->assertEquals('/foo/bar', $c->getUri());
        $this->assertEquals(array('test'), $c->getParamNames());
    }

    public function testCanRemoveParams()
    {
        $c = new Operation(array());
        $c->addParam(new Parameter(array('name' => 'foo')));
        $this->assertTrue($c->hasParam('foo'));
        $c->removeParam('foo');
        $this->assertFalse($c->hasParam('foo'));
    }

    public function testAddsNameToParametersIfNeeded()
    {
        $command = new Operation(array('parameters' => array('foo' => new Parameter(array()))));
        $this->assertEquals('foo', $command->getParam('foo')->getName());
    }

    public function testContainsApiErrorInformation()
    {
        $command = $this->getOperation();
        $this->assertEquals(1, count($command->getErrorResponses()));
        $arr = $command->toArray();
        $this->assertEquals(1, count($arr['errorResponses']));
        $command->addErrorResponse(400, 'Foo', 'Baz\\Bar');
        $this->assertEquals(2, count($command->getErrorResponses()));
        $command->setErrorResponses(array());
        $this->assertEquals(0, count($command->getErrorResponses()));
    }

    public function testHasNotes()
    {
        $o = new Operation(array('notes' => 'foo'));
        $this->assertEquals('foo', $o->getNotes());
        $o->setNotes('bar');
        $this->assertEquals('bar', $o->getNotes());
    }

    public function testHasData()
    {
        $o = new Operation(array('data' => array('foo' => 'baz', 'bar' => 123)));
        $o->setData('test', false);
        $this->assertEquals('baz', $o->getData('foo'));
        $this->assertEquals(123, $o->getData('bar'));
        $this->assertNull($o->getData('wfefwe'));
        $this->assertEquals(array(
            'parameters'    => array(),
            'class'         => 'Guzzle\Service\Command\OperationCommand',
            'data'          => array('foo' => 'baz', 'bar' => 123, 'test' => false),
            'responseClass' => 'array',
            'responseType'  => 'primitive'
        ), $o->toArray());
    }

    public function testHasServiceDescription()
    {
        $s = new ServiceDescription();
        $o = new Operation(array(), $s);
        $this->assertSame($s, $o->getServiceDescription());
    }

    /**
     * @expectedException Guzzle\Common\Exception\InvalidArgumentException
     */
    public function testValidatesResponseType()
    {
        $o = new Operation(array('responseClass' => 'array', 'responseType' => 'foo'));
    }

    public function testInfersResponseType()
    {
        $o = $this->getOperation();
        $o->setServiceDescription(new ServiceDescription(array('models' => array('Foo' => array()))));
        $this->assertEquals('primitive', $o->getResponseType());
        $this->assertEquals('primitive', $o->setResponseClass('boolean')->getResponseType());
        $this->assertEquals('primitive', $o->setResponseClass('array')->getResponseType());
        $this->assertEquals('primitive', $o->setResponseClass('integer')->getResponseType());
        $this->assertEquals('primitive', $o->setResponseClass('string')->getResponseType());
        $this->assertEquals('class', $o->setResponseClass('foo')->getResponseType());
        $this->assertEquals('class', $o->setResponseClass(__CLASS__)->getResponseType());
        $this->assertEquals('model', $o->setResponseClass('Foo')->getResponseType());
    }

    public function testHasResponseType()
    {
        // infers in the constructor
        $o = new Operation(array('responseClass' => 'array'));
        $this->assertEquals('primitive', $o->getResponseType());
        // Infers when set
        $o = new Operation();
        $this->assertEquals('primitive', $o->getResponseType());
        $this->assertEquals('model', $o->setResponseType('model')->getResponseType());
    }

    public function testHasAdditionalParameters()
    {
        $o = new Operation(array(
            'additionalParameters' => array(
                'type' => 'string', 'name' => 'binks'
            ),
            'parameters' => array(
                'foo' => array('type' => 'integer')
            )
        ));
        $this->assertEquals('string', $o->getAdditionalParameters()->getType());
        $arr = $o->toArray();
        $this->assertEquals(array(
            'type' => 'string'
        ), $arr['additionalParameters']);
    }

    /**
     * @return Operation
     */
    protected function getOperation()
    {
        return new Operation(array(
            'name'       => 'OperationTest',
            'class'      => get_class($this),
            'parameters' => array(
                'test'          => array('type' => 'object'),
                'bool_1'        => array('default' => true, 'type' => 'boolean'),
                'bool_2'        => array('default' => false),
                'float'         => array('type' => 'numeric'),
                'int'           => array('type' => 'integer'),
                'date'          => array('type' => 'string'),
                'timestamp'     => array('type' => 'string'),
                'string'        => array('type' => 'string'),
                'username'      => array('type' => 'string', 'required' => true, 'filters' => 'strtolower'),
                'test_function' => array('type' => 'string', 'filters' => __CLASS__ . '::strtoupper')
            ),
            'errorResponses' => array(
                array('code' => 503, 'reason' => 'InsufficientCapacity', 'class' => 'Guzzle\\Exception\\RuntimeException')
            )
        ));
    }
}
