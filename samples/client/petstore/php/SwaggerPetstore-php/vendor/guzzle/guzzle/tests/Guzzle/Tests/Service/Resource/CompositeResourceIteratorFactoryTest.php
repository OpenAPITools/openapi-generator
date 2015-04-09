<?php

namespace Guzzle\Tests\Service\Resource;

use Guzzle\Service\Resource\CompositeResourceIteratorFactory;
use Guzzle\Service\Resource\ResourceIteratorClassFactory;
use Guzzle\Tests\Service\Mock\Command\MockCommand;

/**
 * @covers Guzzle\Service\Resource\CompositeResourceIteratorFactory
 */
class CompositeResourceIteratorFactoryTest extends \Guzzle\Tests\GuzzleTestCase
{
    /**
     * @expectedException \InvalidArgumentException
     * @expectedExceptionMessage Iterator was not found for mock_command
     */
    public function testEnsuresIteratorClassExists()
    {
        $factory = new CompositeResourceIteratorFactory(array(
            new ResourceIteratorClassFactory(array('Foo', 'Bar'))
        ));
        $cmd = new MockCommand();
        $this->assertFalse($factory->canBuild($cmd));
        $factory->build($cmd);
    }

    public function testBuildsResourceIterators()
    {
        $f1 = new ResourceIteratorClassFactory('Guzzle\Tests\Service\Mock\Model');
        $factory = new CompositeResourceIteratorFactory(array());
        $factory->addFactory($f1);
        $command = new MockCommand();
        $iterator = $factory->build($command, array('client.namespace' => 'Guzzle\Tests\Service\Mock'));
        $this->assertInstanceOf('Guzzle\Tests\Service\Mock\Model\MockCommandIterator', $iterator);
    }
}
