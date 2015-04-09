<?php

namespace Guzzle\Tests\Service\Resource;

use Guzzle\Service\Resource\ResourceIteratorClassFactory;
use Guzzle\Tests\Service\Mock\Command\MockCommand;

/**
 * @covers Guzzle\Service\Resource\ResourceIteratorClassFactory
 * @covers Guzzle\Service\Resource\AbstractResourceIteratorFactory
 */
class ResourceIteratorClassFactoryTest extends \Guzzle\Tests\GuzzleTestCase
{
    /**
     * @expectedException \InvalidArgumentException
     * @expectedExceptionMessage Iterator was not found for mock_command
     */
    public function testEnsuresIteratorClassExists()
    {
        $factory = new ResourceIteratorClassFactory(array('Foo', 'Bar'));
        $factory->registerNamespace('Baz');
        $command = new MockCommand();
        $factory->build($command);
    }

    public function testBuildsResourceIterators()
    {
        $factory = new ResourceIteratorClassFactory('Guzzle\Tests\Service\Mock\Model');
        $command = new MockCommand();
        $iterator = $factory->build($command, array('client.namespace' => 'Guzzle\Tests\Service\Mock'));
        $this->assertInstanceOf('Guzzle\Tests\Service\Mock\Model\MockCommandIterator', $iterator);
    }

    public function testChecksIfCanBuild()
    {
        $factory = new ResourceIteratorClassFactory('Guzzle\Tests\Service');
        $this->assertFalse($factory->canBuild(new MockCommand()));
        $factory = new ResourceIteratorClassFactory('Guzzle\Tests\Service\Mock\Model');
        $this->assertTrue($factory->canBuild(new MockCommand()));
    }
}
