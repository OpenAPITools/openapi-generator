<?php

namespace Guzzle\Tests\Service\Command;

use Guzzle\Service\Client;
use Guzzle\Service\Command\Factory\AliasFactory;
use Guzzle\Service\Command\Factory\MapFactory;
use Guzzle\Service\Command\Factory\CompositeFactory;

/**
 * @covers Guzzle\Service\Command\Factory\AliasFactory
 */
class AliasFactoryTest extends \Guzzle\Tests\GuzzleTestCase
{
    private $factory;
    private $client;

    public function setup()
    {
        $this->client = new Client();

        $map = new MapFactory(array(
            'test'  => 'Guzzle\Tests\Service\Mock\Command\MockCommand',
            'test1' => 'Guzzle\Tests\Service\Mock\Command\OtherCommand'
        ));

        $this->factory = new AliasFactory($this->client, array(
            'foo'      => 'test',
            'bar'      => 'sub',
            'sub'      => 'test1',
            'krull'    => 'test3',
            'krull_2'  => 'krull',
            'sub_2'    => 'bar',
            'bad_link' => 'jarjar'
        ));

        $map2 = new MapFactory(array(
            'test3'  => 'Guzzle\Tests\Service\Mock\Command\Sub\Sub'
        ));

        $this->client->setCommandFactory(new CompositeFactory(array($map, $this->factory, $map2)));
    }

    public function aliasProvider()
    {
        return array(
            array('foo', 'Guzzle\Tests\Service\Mock\Command\MockCommand', false),
            array('bar', 'Guzzle\Tests\Service\Mock\Command\OtherCommand', false),
            array('sub', 'Guzzle\Tests\Service\Mock\Command\OtherCommand', false),
            array('sub_2', 'Guzzle\Tests\Service\Mock\Command\OtherCommand', false),
            array('krull', 'Guzzle\Tests\Service\Mock\Command\Sub\Sub', false),
            array('krull_2', 'Guzzle\Tests\Service\Mock\Command\Sub\Sub', false),
            array('missing', null, true),
            array('bad_link', null, true)
        );
    }

    /**
     * @dataProvider aliasProvider
     */
    public function testAliasesCommands($key, $result, $exception)
    {
        try {
            $command = $this->client->getCommand($key);
            if (is_null($result)) {
                $this->assertNull($command);
            } else {
                $this->assertInstanceof($result, $command);
            }
        } catch (\Exception $e) {
            if (!$exception) {
                $this->fail('Got exception when it was not expected');
            }
        }
    }
}
