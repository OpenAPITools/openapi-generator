<?php

namespace Guzzle\Tests\Service\Description;

use Guzzle\Service\Description\ServiceDescription;
use Guzzle\Service\Description\Operation;
use Guzzle\Service\Description\Parameter;
use Guzzle\Service\Client;

/**
 * @covers Guzzle\Service\Description\ServiceDescription
 */
class ServiceDescriptionTest extends \Guzzle\Tests\GuzzleTestCase
{
    protected $serviceData;

    public function setup()
    {
        $this->serviceData = array(
            'test_command' => new Operation(array(
                'name'        => 'test_command',
                'description' => 'documentationForCommand',
                'httpMethod'  => 'DELETE',
                'class'       => 'Guzzle\\Tests\\Service\\Mock\\Command\\MockCommand',
                'parameters'  => array(
                    'bucket'  => array('required' => true),
                    'key'     => array('required' => true)
                )
            ))
        );
    }

    /**
     * @covers Guzzle\Service\Description\ServiceDescription::factory
     * @covers Guzzle\Service\Description\ServiceDescriptionLoader::build
     */
    public function testFactoryDelegatesToConcreteFactories()
    {
        $jsonFile = __DIR__ . '/../../TestData/test_service.json';
        $this->assertInstanceOf('Guzzle\Service\Description\ServiceDescription', ServiceDescription::factory($jsonFile));
    }

    public function testConstructor()
    {
        $service = new ServiceDescription(array('operations' => $this->serviceData));
        $this->assertEquals(1, count($service->getOperations()));
        $this->assertFalse($service->hasOperation('foobar'));
        $this->assertTrue($service->hasOperation('test_command'));
    }

    public function testIsSerializable()
    {
        $service = new ServiceDescription(array('operations' => $this->serviceData));
        $data = serialize($service);
        $d2 = unserialize($data);
        $this->assertEquals(serialize($service), serialize($d2));
    }

    public function testSerializesParameters()
    {
        $service = new ServiceDescription(array(
            'operations' => array(
                'foo' => new Operation(array('parameters' => array('foo' => array('type' => 'string'))))
            )
        ));
        $serialized = serialize($service);
        $this->assertContains('"parameters":{"foo":', $serialized);
        $service = unserialize($serialized);
        $this->assertTrue($service->getOperation('foo')->hasParam('foo'));
    }

    public function testAllowsForJsonBasedArrayParamsFunctionalTest()
    {
        $description = new ServiceDescription(array(
            'operations' => array(
                'test' => new Operation(array(
                    'httpMethod' => 'PUT',
                    'parameters' => array(
                        'data'   => array(
                            'required' => true,
                            'filters'  => 'json_encode',
                            'location' => 'body'
                        )
                    )
                ))
            )
        ));
        $client = new Client();
        $client->setDescription($description);
        $command = $client->getCommand('test', array(
            'data' => array(
                'foo' => 'bar'
            )
        ));

        $request = $command->prepare();
        $this->assertEquals('{"foo":"bar"}', (string) $request->getBody());
    }

    public function testContainsModels()
    {
        $d = new ServiceDescription(array(
            'operations' => array('foo' => array()),
            'models' => array(
                'Tag'    => array('type' => 'object'),
                'Person' => array('type' => 'object')
            )
        ));
        $this->assertTrue($d->hasModel('Tag'));
        $this->assertTrue($d->hasModel('Person'));
        $this->assertFalse($d->hasModel('Foo'));
        $this->assertInstanceOf('Guzzle\Service\Description\Parameter', $d->getModel('Tag'));
        $this->assertNull($d->getModel('Foo'));
        $this->assertContains('"models":{', serialize($d));
        $this->assertEquals(array('Tag', 'Person'), array_keys($d->getModels()));
    }

    public function testCanAddModels()
    {
        $d = new ServiceDescription(array());
        $this->assertFalse($d->hasModel('Foo'));
        $d->addModel(new Parameter(array('name' => 'Foo')));
        $this->assertTrue($d->hasModel('Foo'));
    }

    public function testHasAttributes()
    {
        $d = new ServiceDescription(array(
            'operations'  => array(),
            'name'        => 'Name',
            'description' => 'Description',
            'apiVersion'  => '1.24'
        ));

        $this->assertEquals('Name', $d->getName());
        $this->assertEquals('Description', $d->getDescription());
        $this->assertEquals('1.24', $d->getApiVersion());

        $s = serialize($d);
        $this->assertContains('"name":"Name"', $s);
        $this->assertContains('"description":"Description"', $s);
        $this->assertContains('"apiVersion":"1.24"', $s);

        $d = unserialize($s);
        $this->assertEquals('Name', $d->getName());
        $this->assertEquals('Description', $d->getDescription());
        $this->assertEquals('1.24', $d->getApiVersion());
    }

    public function testPersistsCustomAttributes()
    {
        $data = array(
            'operations'  => array('foo' => array('class' => 'foo', 'parameters' => array())),
            'name'        => 'Name',
            'description' => 'Test',
            'apiVersion'  => '1.24',
            'auth'        => 'foo',
            'keyParam'    => 'bar'
        );
        $d = new ServiceDescription($data);
        $d->setData('hello', 'baz');
        $this->assertEquals('foo', $d->getData('auth'));
        $this->assertEquals('baz', $d->getData('hello'));
        $this->assertEquals('bar', $d->getData('keyParam'));
        // responseClass and responseType are added by default
        $data['operations']['foo']['responseClass'] = 'array';
        $data['operations']['foo']['responseType'] = 'primitive';
        $this->assertEquals($data + array('hello' => 'baz'), json_decode($d->serialize(), true));
    }

    public function testHasToArray()
    {
        $data = array(
            'operations'  => array(),
            'name'        => 'Name',
            'description' => 'Test'
        );
        $d = new ServiceDescription($data);
        $arr = $d->toArray();
        $this->assertEquals('Name', $arr['name']);
        $this->assertEquals('Test', $arr['description']);
    }

    public function testReturnsNullWhenRetrievingMissingOperation()
    {
        $s = new ServiceDescription(array());
        $this->assertNull($s->getOperation('foo'));
    }

    public function testCanAddOperations()
    {
        $s = new ServiceDescription(array());
        $this->assertFalse($s->hasOperation('Foo'));
        $s->addOperation(new Operation(array('name' => 'Foo')));
        $this->assertTrue($s->hasOperation('Foo'));
    }

    /**
     * @expectedException Guzzle\Common\Exception\InvalidArgumentException
     */
    public function testValidatesOperationTypes()
    {
        $s = new ServiceDescription(array(
            'operations' => array('foo' => new \stdClass())
        ));
    }

    public function testHasBaseUrl()
    {
        $description = new ServiceDescription(array('baseUrl' => 'http://foo.com'));
        $this->assertEquals('http://foo.com', $description->getBaseUrl());
        $description->setBaseUrl('http://foobar.com');
        $this->assertEquals('http://foobar.com', $description->getBaseUrl());
    }

    public function testCanUseBasePath()
    {
        $description = new ServiceDescription(array('basePath' => 'http://foo.com'));
        $this->assertEquals('http://foo.com', $description->getBaseUrl());
    }

    public function testModelsHaveNames()
    {
        $desc = array(
            'models' => array(
                'date' => array('type' => 'string'),
                'user'=> array(
                    'type' => 'object',
                    'properties' => array(
                        'dob' => array('$ref' => 'date')
                    )
                )
            )
        );

        $s = ServiceDescription::factory($desc);
        $this->assertEquals('date', $s->getModel('date')->getName());
        $this->assertEquals('dob', $s->getModel('user')->getProperty('dob')->getName());
    }
}
