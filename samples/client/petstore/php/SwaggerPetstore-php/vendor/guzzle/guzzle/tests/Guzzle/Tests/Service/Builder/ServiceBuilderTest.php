<?php

namespace Guzzle\Tests\Service;

use Guzzle\Plugin\History\HistoryPlugin;
use Guzzle\Service\Builder\ServiceBuilder;
use Guzzle\Service\Client;

/**
 * @covers Guzzle\Service\Builder\ServiceBuilder
 */
class ServiceBuilderTest extends \Guzzle\Tests\GuzzleTestCase
{
    protected $arrayData = array(
        'michael.mock' => array(
            'class' => 'Guzzle\Tests\Service\Mock\MockClient',
            'params' => array(
                'username' => 'michael',
                'password' => 'testing123',
                'subdomain' => 'michael',
            ),
        ),
        'billy.mock' => array(
            'alias' => 'Hello!',
            'class' => 'Guzzle\Tests\Service\Mock\MockClient',
            'params' => array(
                'username' => 'billy',
                'password' => 'passw0rd',
                'subdomain' => 'billy',
            ),
        ),
        'billy.testing' => array(
            'extends' => 'billy.mock',
            'params' => array(
                'subdomain' => 'test.billy',
            ),
        ),
        'missing_params' => array(
            'extends' => 'billy.mock'
        )
    );

    public function testAllowsSerialization()
    {
        $builder = ServiceBuilder::factory($this->arrayData);
        $cached = unserialize(serialize($builder));
        $this->assertEquals($cached, $builder);
    }

    public function testDelegatesFactoryMethodToAbstractFactory()
    {
        $builder = ServiceBuilder::factory($this->arrayData);
        $c = $builder->get('michael.mock');
        $this->assertInstanceOf('Guzzle\Tests\Service\Mock\MockClient', $c);
    }

    /**
     * @expectedException Guzzle\Service\Exception\ServiceNotFoundException
     * @expectedExceptionMessage No service is registered as foobar
     */
    public function testThrowsExceptionWhenGettingInvalidClient()
    {
        ServiceBuilder::factory($this->arrayData)->get('foobar');
    }

    public function testStoresClientCopy()
    {
        $builder = ServiceBuilder::factory($this->arrayData);
        $client = $builder->get('michael.mock');
        $this->assertInstanceOf('Guzzle\Tests\Service\Mock\MockClient', $client);
        $this->assertEquals('http://127.0.0.1:8124/v1/michael', $client->getBaseUrl());
        $this->assertEquals($client, $builder->get('michael.mock'));

        // Get another client but throw this one away
        $client2 = $builder->get('billy.mock', true);
        $this->assertInstanceOf('Guzzle\Tests\Service\Mock\MockClient', $client2);
        $this->assertEquals('http://127.0.0.1:8124/v1/billy', $client2->getBaseUrl());

        // Make sure the original client is still there and set
        $this->assertTrue($client === $builder->get('michael.mock'));

        // Create a new billy.mock client that is stored
        $client3 = $builder->get('billy.mock');

        // Make sure that the stored billy.mock client is equal to the other stored client
        $this->assertTrue($client3 === $builder->get('billy.mock'));

        // Make sure that this client is not equal to the previous throwaway client
        $this->assertFalse($client2 === $builder->get('billy.mock'));
    }

    public function testBuildersPassOptionsThroughToClients()
    {
        $s = new ServiceBuilder(array(
            'michael.mock' => array(
                'class' => 'Guzzle\Tests\Service\Mock\MockClient',
                'params' => array(
                    'base_url' => 'http://www.test.com/',
                    'subdomain' => 'michael',
                    'password' => 'test',
                    'username' => 'michael',
                    'curl.curlopt_proxyport' => 8080
                )
            )
        ));

        $c = $s->get('michael.mock');
        $this->assertEquals(8080, $c->getConfig('curl.curlopt_proxyport'));
    }

    public function testUsesTheDefaultBuilderWhenNoBuilderIsSpecified()
    {
        $s = new ServiceBuilder(array(
            'michael.mock' => array(
                'class' => 'Guzzle\Tests\Service\Mock\MockClient',
                'params' => array(
                    'base_url' => 'http://www.test.com/',
                    'subdomain' => 'michael',
                    'password' => 'test',
                    'username' => 'michael',
                    'curl.curlopt_proxyport' => 8080
                )
            )
        ));

        $c = $s->get('michael.mock');
        $this->assertInstanceOf('Guzzle\Tests\Service\Mock\MockClient', $c);
    }

    public function testUsedAsArray()
    {
        $b = ServiceBuilder::factory($this->arrayData);
        $this->assertTrue($b->offsetExists('michael.mock'));
        $this->assertFalse($b->offsetExists('not_there'));
        $this->assertInstanceOf('Guzzle\Service\Client', $b['michael.mock']);

        unset($b['michael.mock']);
        $this->assertFalse($b->offsetExists('michael.mock'));

        $b['michael.mock'] = new Client('http://www.test.com/');
        $this->assertInstanceOf('Guzzle\Service\Client', $b['michael.mock']);
    }

    public function testFactoryCanCreateFromJson()
    {
        $tmp = sys_get_temp_dir() . '/test.js';
        file_put_contents($tmp, json_encode($this->arrayData));
        $b = ServiceBuilder::factory($tmp);
        unlink($tmp);
        $s = $b->get('billy.testing');
        $this->assertEquals('test.billy', $s->getConfig('subdomain'));
        $this->assertEquals('billy', $s->getConfig('username'));
    }

    public function testFactoryCanCreateFromArray()
    {
        $b = ServiceBuilder::factory($this->arrayData);
        $s = $b->get('billy.testing');
        $this->assertEquals('test.billy', $s->getConfig('subdomain'));
        $this->assertEquals('billy', $s->getConfig('username'));
    }

    public function testFactoryDoesNotRequireParams()
    {
        $b = ServiceBuilder::factory($this->arrayData);
        $s = $b->get('missing_params');
        $this->assertEquals('billy', $s->getConfig('username'));
    }

    public function testBuilderAllowsReferencesBetweenClients()
    {
        $builder = ServiceBuilder::factory(array(
            'a' => array(
                'class' => 'Guzzle\Tests\Service\Mock\MockClient',
                'params' => array(
                    'other_client' => '{b}',
                    'username'     => 'x',
                    'password'     => 'y',
                    'subdomain'    => 'z'
                )
            ),
            'b' => array(
                'class' => 'Guzzle\Tests\Service\Mock\MockClient',
                'params' => array(
                    'username'  => '1',
                    'password'  => '2',
                    'subdomain' => '3'
                )
            )
        ));

        $client = $builder['a'];
        $this->assertEquals('x', $client->getConfig('username'));
        $this->assertSame($builder['b'], $client->getConfig('other_client'));
        $this->assertEquals('1', $builder['b']->getConfig('username'));
    }

    public function testEmitsEventsWhenClientsAreCreated()
    {
        // Ensure that the client signals that it emits an event
        $this->assertEquals(array('service_builder.create_client'), ServiceBuilder::getAllEvents());

        // Create a test service builder
        $builder = ServiceBuilder::factory(array(
            'a' => array(
                'class' => 'Guzzle\Tests\Service\Mock\MockClient',
                'params' => array(
                    'username'  => 'test',
                    'password'  => '123',
                    'subdomain' => 'z'
                )
            )
        ));

        // Add an event listener to pick up client creation events
        $emits = 0;
        $builder->getEventDispatcher()->addListener('service_builder.create_client', function($event) use (&$emits) {
            $emits++;
        });

        // Get the 'a' client by name
        $client = $builder->get('a');

        // Ensure that the event was emitted once, and that the client was present
        $this->assertEquals(1, $emits);
        $this->assertInstanceOf('Guzzle\Tests\Service\Mock\MockClient', $client);
    }

    public function testCanAddGlobalParametersToServicesOnLoad()
    {
        $builder = ServiceBuilder::factory($this->arrayData, array(
            'username'  => 'fred',
            'new_value' => 'test'
        ));

        $data = json_decode($builder->serialize(), true);

        foreach ($data as $service) {
            $this->assertEquals('fred', $service['params']['username']);
            $this->assertEquals('test', $service['params']['new_value']);
        }
    }

    public function testAddsGlobalPlugins()
    {
        $b = new ServiceBuilder($this->arrayData);
        $b->addGlobalPlugin(new HistoryPlugin());
        $s = $b->get('michael.mock');
        $this->assertTrue($s->getEventDispatcher()->hasListeners('request.sent'));
    }

    public function testCanGetData()
    {
        $b = new ServiceBuilder($this->arrayData);
        $this->assertEquals($this->arrayData['michael.mock'], $b->getData('michael.mock'));
        $this->assertNull($b->getData('ewofweoweofe'));
    }

    public function testCanGetByAlias()
    {
        $b = new ServiceBuilder($this->arrayData);
        $this->assertSame($b->get('billy.mock'), $b->get('Hello!'));
    }

    public function testCanOverwriteParametersForThrowawayClients()
    {
        $b = new ServiceBuilder($this->arrayData);

        $c1 = $b->get('michael.mock');
        $this->assertEquals('michael', $c1->getConfig('username'));

        $c2 = $b->get('michael.mock', array('username' => 'jeremy'));
        $this->assertEquals('jeremy', $c2->getConfig('username'));
    }

    public function testGettingAThrowawayClientWithParametersDoesNotAffectGettingOtherClients()
    {
        $b = new ServiceBuilder($this->arrayData);

        $c1 = $b->get('michael.mock', array('username' => 'jeremy'));
        $this->assertEquals('jeremy', $c1->getConfig('username'));

        $c2 = $b->get('michael.mock');
        $this->assertEquals('michael', $c2->getConfig('username'));
    }

    public function testCanUseArbitraryData()
    {
        $b = new ServiceBuilder();
        $b['a'] = 'foo';
        $this->assertTrue(isset($b['a']));
        $this->assertEquals('foo', $b['a']);
        unset($b['a']);
        $this->assertFalse(isset($b['a']));
    }

    public function testCanRegisterServiceData()
    {
        $b = new ServiceBuilder();
        $b['a'] = array(
            'class' => 'Guzzle\Tests\Service\Mock\MockClient',
            'params' => array(
                'username' => 'billy',
                'password' => 'passw0rd',
                'subdomain' => 'billy',
            )
        );
        $this->assertTrue(isset($b['a']));
        $this->assertInstanceOf('Guzzle\Tests\Service\Mock\MockClient', $b['a']);
        $client = $b['a'];
        unset($b['a']);
        $this->assertFalse(isset($b['a']));
        // Ensure that instantiated clients can be registered
        $b['mock'] = $client;
        $this->assertSame($client, $b['mock']);
    }
}
