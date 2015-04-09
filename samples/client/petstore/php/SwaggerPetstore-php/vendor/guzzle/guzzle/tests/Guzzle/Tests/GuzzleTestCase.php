<?php

namespace Guzzle\Tests;

use Guzzle\Common\HasDispatcherInterface;
use Guzzle\Common\Event;
use Guzzle\Http\Message\Response;
use Guzzle\Http\Message\RequestInterface;
use Guzzle\Tests\Http\Message\HeaderComparison;
use Guzzle\Plugin\Mock\MockPlugin;
use Guzzle\Http\Client;
use Guzzle\Service\Builder\ServiceBuilderInterface;
use Guzzle\Service\Builder\ServiceBuilder;
use Guzzle\Tests\Mock\MockObserver;
use Guzzle\Tests\Http\Server;
use RuntimeException;
use Symfony\Component\EventDispatcher\EventSubscriberInterface;

/**
 * Base testcase class for all Guzzle testcases.
 */
abstract class GuzzleTestCase extends \PHPUnit_Framework_TestCase
{
    protected static $mockBasePath;
    public static $serviceBuilder;
    public static $server;

    private $requests = array();
    public $mockObserver;

    /**
     * Get the global server object used throughout the unit tests of Guzzle
     *
     * @return Server
     */
    public static function getServer()
    {
        if (!self::$server) {
            self::$server = new Server();
            if (self::$server->isRunning()) {
                self::$server->flush();
            } else {
                self::$server->start();
            }
        }

        return self::$server;
    }

    /**
     * Set the service builder to use for tests
     *
     * @param ServiceBuilderInterface $builder Service builder
     */
    public static function setServiceBuilder(ServiceBuilderInterface $builder)
    {
        self::$serviceBuilder = $builder;
    }

    /**
     * Get a service builder object that can be used throughout the service tests
     *
     * @return ServiceBuilder
     */
    public static function getServiceBuilder()
    {
        if (!self::$serviceBuilder) {
            throw new RuntimeException('No service builder has been set via setServiceBuilder()');
        }

        return self::$serviceBuilder;
    }

    /**
     * Check if an event dispatcher has a subscriber
     *
     * @param HasDispatcherInterface $dispatcher
     * @param EventSubscriberInterface $subscriber
     *
     * @return bool
     */
    protected function hasSubscriber(HasDispatcherInterface $dispatcher, EventSubscriberInterface $subscriber)
    {
        $class = get_class($subscriber);
        $all = array_keys(call_user_func(array($class, 'getSubscribedEvents')));

        foreach ($all as $i => $event) {
            foreach ($dispatcher->getEventDispatcher()->getListeners($event) as $e) {
                if ($e[0] === $subscriber) {
                    unset($all[$i]);
                    break;
                }
            }
        }

        return count($all) == 0;
    }

    /**
     * Get a wildcard observer for an event dispatcher
     *
     * @param HasDispatcherInterface $hasDispatcher
     *
     * @return MockObserver
     */
    public function getWildcardObserver(HasDispatcherInterface $hasDispatcher)
    {
        $class = get_class($hasDispatcher);
        $o = new MockObserver();
        $events = call_user_func(array($class, 'getAllEvents'));
        foreach ($events as $event) {
            $hasDispatcher->getEventDispatcher()->addListener($event, array($o, 'update'));
        }

        return $o;
    }

    /**
     * Set the mock response base path
     *
     * @param string $path Path to mock response folder
     *
     * @return GuzzleTestCase
     */
    public static function setMockBasePath($path)
    {
        self::$mockBasePath = $path;
    }

    /**
     * Mark a request as being mocked
     *
     * @param RequestInterface $request
     *
     * @return self
     */
    public function addMockedRequest(RequestInterface $request)
    {
        $this->requests[] = $request;

        return $this;
    }

    /**
     * Get all of the mocked requests
     *
     * @return array
     */
    public function getMockedRequests()
    {
        return $this->requests;
    }

    /**
     * Get a mock response for a client by mock file name
     *
     * @param string $path Relative path to the mock response file
     *
     * @return Response
     */
    public function getMockResponse($path)
    {
        return $path instanceof Response
            ? $path
            : MockPlugin::getMockFile(self::$mockBasePath . DIRECTORY_SEPARATOR . $path);
    }

    /**
     * Set a mock response from a mock file on the next client request.
     *
     * This method assumes that mock response files are located under the
     * Command/Mock/ directory of the Service being tested
     * (e.g. Unfuddle/Command/Mock/).  A mock response is added to the next
     * request sent by the client.
     *
     * @param Client $client Client object to modify
     * @param string $paths  Path to files within the Mock folder of the service
     *
     * @return MockPlugin returns the created mock plugin
     */
    public function setMockResponse(Client $client, $paths)
    {
        $this->requests = array();
        $that = $this;
        $mock = new MockPlugin(null, true);
        $client->getEventDispatcher()->removeSubscriber($mock);
        $mock->getEventDispatcher()->addListener('mock.request', function(Event $event) use ($that) {
            $that->addMockedRequest($event['request']);
        });

        if ($paths instanceof Response) {
            // A single response instance has been specified, create an array with that instance
            // as the only element for the following loop to work as expected
            $paths = array($paths);
        }

        foreach ((array) $paths as $path) {
            $mock->addResponse($this->getMockResponse($path));
        }

        $client->getEventDispatcher()->addSubscriber($mock);

        return $mock;
    }

    /**
     * Compare HTTP headers and use special markup to filter values
     * A header prefixed with '!' means it must not exist
     * A header prefixed with '_' means it must be ignored
     * A header value of '*' means anything after the * will be ignored
     *
     * @param array $filteredHeaders Array of special headers
     * @param array $actualHeaders Array of headers to check against
     *
     * @return array|bool Returns an array of the differences or FALSE if none
     */
    public function compareHeaders($filteredHeaders, $actualHeaders)
    {
        $comparison = new HeaderComparison();

        return $comparison->compare($filteredHeaders, $actualHeaders);
    }

    /**
     * Case insensitive assertContains
     *
     * @param string $needle Search string
     * @param string $haystack Search this
     * @param string $message Optional failure message
     */
    public function assertContainsIns($needle, $haystack, $message = null)
    {
        $this->assertContains(strtolower($needle), strtolower($haystack), $message);
    }
}
