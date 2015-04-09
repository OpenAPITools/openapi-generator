<?php

namespace Guzzle\Tests\Service\Command;

use Guzzle\Tests\Service\Mock\MockClient;
use Guzzle\Service\Command\Factory\ConcreteClassFactory;

/**
 * @covers Guzzle\Service\Command\Factory\ConcreteClassFactory
 */
class ConcreteClassFactoryTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testProvider()
    {
        return array(
            array('foo', null, 'Guzzle\\Tests\\Service\\Mock\\Command\\'),
            array('mock_command', 'Guzzle\Tests\Service\Mock\Command\MockCommand', 'Guzzle\\Tests\\Service\\Mock\\Command\\'),
            array('other_command', 'Guzzle\Tests\Service\Mock\Command\OtherCommand', 'Guzzle\\Tests\\Service\\Mock\\Command\\'),
            array('sub.sub', 'Guzzle\Tests\Service\Mock\Command\Sub\Sub', 'Guzzle\\Tests\\Service\\Mock\\Command\\'),
            array('sub.sub', null, 'Guzzle\\Foo\\'),
            array('foo', null, null),
            array('mock_command', 'Guzzle\Tests\Service\Mock\Command\MockCommand', null),
            array('other_command', 'Guzzle\Tests\Service\Mock\Command\OtherCommand', null),
            array('sub.sub', 'Guzzle\Tests\Service\Mock\Command\Sub\Sub', null)
        );
    }

    /**
     * @dataProvider testProvider
     */
    public function testCreatesConcreteCommands($key, $result, $prefix)
    {
        if (!$prefix) {
            $client = new MockClient();
        } else {
            $client = new MockClient('', array(
                'command.prefix' => $prefix
            ));
        }

        $factory = new ConcreteClassFactory($client);

        if (is_null($result)) {
            $this->assertNull($factory->factory($key));
        } else {
            $this->assertInstanceof($result, $factory->factory($key));
        }
    }
}
