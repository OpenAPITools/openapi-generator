<?php

namespace Guzzle\Tests\Service\Description;

use Guzzle\Service\Description\ServiceDescription;
use Guzzle\Service\Description\ServiceDescriptionLoader;

/**
 * @covers Guzzle\Service\Description\ServiceDescriptionLoader
 */
class ServiceDescriptionLoaderTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testAllowsExtraData()
    {
        $d = ServiceDescription::factory(array(
            'foo' => true,
            'baz' => array('bar'),
            'apiVersion' => '123',
            'operations' => array()
        ));

        $this->assertEquals(true, $d->getData('foo'));
        $this->assertEquals(array('bar'), $d->getData('baz'));
        $this->assertEquals('123', $d->getApiVersion());
    }

    public function testAllowsDeepNestedInheritance()
    {
        $d = ServiceDescription::factory(array(
            'operations' => array(
                'abstract' => array(
                    'httpMethod' => 'HEAD',
                    'parameters' => array(
                        'test' => array('type' => 'string', 'required' => true)
                    )
                ),
                'abstract2' => array('uri' => '/test', 'extends' => 'abstract'),
                'concrete'  => array('extends' => 'abstract2'),
                'override'  => array('extends' => 'abstract', 'httpMethod' => 'PUT'),
                'override2'  => array('extends' => 'override', 'httpMethod' => 'POST', 'uri' => '/')
            )
        ));

        $c = $d->getOperation('concrete');
        $this->assertEquals('/test', $c->getUri());
        $this->assertEquals('HEAD', $c->getHttpMethod());
        $params = $c->getParams();
        $param = $params['test'];
        $this->assertEquals('string', $param->getType());
        $this->assertTrue($param->getRequired());

        // Ensure that merging HTTP method does not make an array
        $this->assertEquals('PUT', $d->getOperation('override')->getHttpMethod());
        $this->assertEquals('POST', $d->getOperation('override2')->getHttpMethod());
        $this->assertEquals('/', $d->getOperation('override2')->getUri());
    }

    /**
     * @expectedException RuntimeException
     */
    public function testThrowsExceptionWhenExtendingMissingCommand()
    {
        ServiceDescription::factory(array(
            'operations' => array(
                'concrete' => array(
                    'extends' => 'missing'
                )
            )
        ));
    }

    public function testAllowsMultipleInheritance()
    {
        $description = ServiceDescription::factory(array(
            'operations' => array(
                'a' => array(
                    'httpMethod' => 'GET',
                    'parameters' => array(
                        'a1' => array(
                            'default'  => 'foo',
                            'required' => true,
                            'prepend'  => 'hi'
                        )
                    )
                ),
                'b' => array(
                    'extends' => 'a',
                    'parameters' => array(
                        'b2' => array()
                    )
                ),
                'c' => array(
                    'parameters' => array(
                        'a1' => array(
                            'default'     => 'bar',
                            'required'    => true,
                            'description' => 'test'
                        ),
                        'c3' => array()
                    )
                ),
                'd' => array(
                    'httpMethod' => 'DELETE',
                    'extends'    => array('b', 'c'),
                    'parameters' => array(
                        'test' => array()
                    )
                )
            )
        ));

        $command = $description->getOperation('d');
        $this->assertEquals('DELETE', $command->getHttpMethod());
        $this->assertContains('a1', $command->getParamNames());
        $this->assertContains('b2', $command->getParamNames());
        $this->assertContains('c3', $command->getParamNames());
        $this->assertContains('test', $command->getParamNames());

        $this->assertTrue($command->getParam('a1')->getRequired());
        $this->assertEquals('bar', $command->getParam('a1')->getDefault());
        $this->assertEquals('test', $command->getParam('a1')->getDescription());
    }

    public function testAddsOtherFields()
    {
        $description = ServiceDescription::factory(array(
            'operations'  => array(),
            'description' => 'Foo',
            'apiVersion'  => 'bar'
        ));
        $this->assertEquals('Foo', $description->getDescription());
        $this->assertEquals('bar', $description->getApiVersion());
    }

    public function testCanLoadNestedExtends()
    {
        $description = ServiceDescription::factory(array(
            'operations'  => array(
                'root' => array(
                    'class' => 'foo'
                ),
                'foo' => array(
                    'extends' => 'root',
                    'parameters' => array(
                        'baz' => array('type' => 'string')
                    )
                ),
                'foo_2' => array(
                    'extends' => 'foo',
                    'parameters' => array(
                        'bar' => array('type' => 'string')
                    )
                ),
                'foo_3' => array(
                    'class' => 'bar',
                    'parameters' => array(
                        'bar2' => array('type' => 'string')
                    )
                ),
                'foo_4' => array(
                    'extends' => array('foo_2', 'foo_3'),
                    'parameters' => array(
                        'bar3' => array('type' => 'string')
                    )
                )
            )
        ));

        $this->assertTrue($description->hasOperation('foo_4'));
        $foo4 = $description->getOperation('foo_4');
        $this->assertTrue($foo4->hasParam('baz'));
        $this->assertTrue($foo4->hasParam('bar'));
        $this->assertTrue($foo4->hasParam('bar2'));
        $this->assertTrue($foo4->hasParam('bar3'));
        $this->assertEquals('bar', $foo4->getClass());
    }
}
