<?php

namespace Guzzle\Tests\Service\Resource;

use Guzzle\Service\Resource\MapResourceIteratorFactory;
use Guzzle\Tests\Service\Mock\Command\MockCommand;

/**
 * @covers Guzzle\Service\Resource\MapResourceIteratorFactory
 */
class MapResourceIteratorFactoryTest extends \Guzzle\Tests\GuzzleTestCase
{
    /**
     * @expectedException InvalidArgumentException
     * @expectedExceptionMessage Iterator was not found for mock_command
     */
    public function testEnsuresIteratorClassExists()
    {
        $factory = new MapResourceIteratorFactory(array('Foo', 'Bar'));
        $factory->build(new MockCommand());
    }

    public function testBuildsResourceIterators()
    {
        $factory = new MapResourceIteratorFactory(array(
            'mock_command' => 'Guzzle\Tests\Service\Mock\Model\MockCommandIterator'
        ));
        $iterator = $factory->build(new MockCommand());
        $this->assertInstanceOf('Guzzle\Tests\Service\Mock\Model\MockCommandIterator', $iterator);
    }

    public function testUsesWildcardMappings()
    {
        $factory = new MapResourceIteratorFactory(array(
            '*' => 'Guzzle\Tests\Service\Mock\Model\MockCommandIterator'
        ));
        $iterator = $factory->build(new MockCommand());
        $this->assertInstanceOf('Guzzle\Tests\Service\Mock\Model\MockCommandIterator', $iterator);
    }
}
