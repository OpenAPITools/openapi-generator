<?php

namespace Guzzle\Tests\Plugin\Cache;

use Guzzle\Cache\DoctrineCacheAdapter;
use Guzzle\Http\Message\Request;
use Guzzle\Http\Message\RequestFactory;
use Guzzle\Http\Message\Response;
use Guzzle\Plugin\Cache\DefaultCacheStorage;
use Doctrine\Common\Cache\ArrayCache;

/**
 * @covers Guzzle\Plugin\Cache\DefaultCacheStorage
 */
class DefaultCacheStorageTest extends \Guzzle\Tests\GuzzleTestCase
{
    protected function getCache()
    {
        $a = new ArrayCache();
        $c = new DoctrineCacheAdapter($a);
        $s = new DefaultCacheStorage($c);
        $request = new Request('GET', 'http://foo.com', array('Accept' => 'application/json'));
        $response = new Response(200, array(
            'Content-Type' => 'application/json',
            'Connection' => 'close',
            'X-Foo' => 'Bar',
            'Vary' => 'Accept'
        ), 'test');
        $s->cache($request, $response);
        $data = $this->readAttribute($a, 'data');

        return array(
            'cache' => $a,
            'adapter' => $c,
            'storage' => $s,
            'request' => $request,
            'response' => $response,
            'serialized' => end($data)
        );
    }

    public function testReturnsNullForCacheMiss()
    {
        $cache = $this->getCache();
        $this->assertNull($cache['storage']->fetch(new Request('GET', 'http://test.com')));
    }

    public function testCachesRequests()
    {
        $cache = $this->getCache();
        $foundRequest = $foundBody = $bodyKey = false;
        foreach ($this->readAttribute($cache['cache'], 'data') as $key => $v) {
            if (strpos($v, 'foo.com')) {
                $foundRequest = true;
                $data = unserialize($v);
                $bodyKey = $data[0][3];
                $this->assertInternalType('integer', $data[0][4]);
                $this->assertFalse(isset($data[0][0]['connection']));
                $this->assertEquals('foo.com', $data[0][0]['host']);
            } elseif ($v == 'test') {
                $foundBody = $key;
            }
        }
        $this->assertContains($bodyKey, $foundBody);
        $this->assertTrue($foundRequest);
    }

    public function testFetchesResponse()
    {
        $cache = $this->getCache();
        $response = $cache['storage']->fetch($cache['request']);
        $this->assertEquals(200, $response->getStatusCode());
        $this->assertFalse($response->hasHeader('Connection'));
        $this->assertEquals('Bar', (string) $response->getHeader('X-Foo'));
        $this->assertEquals('test', (string) $response->getBody());
        $this->assertTrue(in_array($cache['serialized'], $this->readAttribute($cache['cache'], 'data')));
    }

    public function testDeletesRequestItemsAndBody()
    {
        $cache = $this->getCache();
        $cache['storage']->delete($cache['request']);
        $this->assertFalse(in_array('test', $this->readAttribute($cache['cache'], 'data')));
        $this->assertFalse(in_array($cache['serialized'], $this->readAttribute($cache['cache'], 'data')));
    }

    public function testCachesMultipleRequestsWithVary()
    {
        $cache = $this->getCache();
        $cache['request']->setHeader('Accept', 'application/xml');
        $response = $cache['response']->setHeader('Content-Type', 'application/xml');
        $response->setBody('123');
        $cache['storage']->cache($cache['request'], $response);
        $data = $this->readAttribute($cache['cache'], 'data');
        foreach ($data as $v) {
            if (strpos($v, 'foo.com')) {
                $u = unserialize($v);
                $this->assertEquals(2, count($u));
                $this->assertEquals($u[0][0]['accept'], 'application/xml');
                $this->assertEquals($u[0][1]['content-type'], 'application/xml');
                $this->assertEquals($u[1][0]['accept'], 'application/json');
                $this->assertEquals($u[1][1]['content-type'], 'application/json');
                $this->assertNotSame($u[0][3], $u[1][3]);
                break;
            }
        }
    }

    public function testPurgeRemovesAllMethodCaches()
    {
        $cache = $this->getCache();
        foreach (array('HEAD', 'POST', 'PUT', 'DELETE') as $method) {
            $request = RequestFactory::getInstance()->cloneRequestWithMethod($cache['request'], $method);
            $cache['storage']->cache($request, $cache['response']);
        }
        $cache['storage']->purge('http://foo.com');
        $this->assertFalse(in_array('test', $this->readAttribute($cache['cache'], 'data')));
        $this->assertFalse(in_array($cache['serialized'], $this->readAttribute($cache['cache'], 'data')));
        $this->assertEquals(
            array('DoctrineNamespaceCacheKey[]'),
            array_keys($this->readAttribute($cache['cache'], 'data'))
        );
    }

    public function testRemovesExpiredResponses()
    {
        $cache = $this->getCache();
        $request = new Request('GET', 'http://xyz.com');
        $response = new Response(200, array('Age' => 1000, 'Cache-Control' => 'max-age=-10000'));
        $cache['storage']->cache($request, $response);
        $this->assertNull($cache['storage']->fetch($request));
        $data = $this->readAttribute($cache['cache'], 'data');
        $this->assertFalse(in_array('xyz.com', $data));
        $this->assertTrue(in_array($cache['serialized'], $data));
    }

    public function testUsesVaryToDetermineResult()
    {
        $cache = $this->getCache();
        $this->assertInstanceOf('Guzzle\Http\Message\Response', $cache['storage']->fetch($cache['request']));
        $request = new Request('GET', 'http://foo.com', array('Accept' => 'application/xml'));
        $this->assertNull($cache['storage']->fetch($request));
    }

    public function testEnsuresResponseIsStillPresent()
    {
        $cache = $this->getCache();
        $data = $this->readAttribute($cache['cache'], 'data');
        $key = array_search('test', $data);
        $cache['cache']->delete(substr($key, 1, -4));
        $this->assertNull($cache['storage']->fetch($cache['request']));
    }

    public function staleProvider()
    {
        return array(
            array(
                new Request('GET', 'http://foo.com', array('Accept' => 'foo')),
                new Response(200, array('Cache-Control' => 'stale-if-error=100', 'Vary' => 'Accept'))
            ),
            array(
                new Request('GET', 'http://foo.com', array('Accept' => 'foo')),
                new Response(200, array('Cache-Control' => 'stale-if-error', 'Vary' => 'Accept'))
            )
        );
    }

    /**
     * @dataProvider staleProvider
     */
    public function testUsesStaleTimeDirectiveForTtd($request, $response)
    {
        $cache = $this->getCache();
        $cache['storage']->cache($request, $response);
        $data = $this->readAttribute($cache['cache'], 'data');
        foreach ($data as $v) {
            if (strpos($v, 'foo.com')) {
                $u = unserialize($v);
                $this->assertGreaterThan($u[1][4], $u[0][4]);
                break;
            }
        }
    }

    public function testCanFilterCacheKeys()
    {
        $cache = $this->getCache();
        $cache['request']->getQuery()->set('auth', 'foo');
        $this->assertNull($cache['storage']->fetch($cache['request']));
        $cache['request']->getParams()->set('cache.key_filter', 'auth');
        $this->assertNotNull($cache['storage']->fetch($cache['request']));
    }
}
