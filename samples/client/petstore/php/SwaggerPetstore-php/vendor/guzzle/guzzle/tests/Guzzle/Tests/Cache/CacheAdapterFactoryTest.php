<?php

namespace Guzzle\Tests\Cache;

use Guzzle\Cache\CacheAdapterFactory;
use Guzzle\Cache\DoctrineCacheAdapter;
use Doctrine\Common\Cache\ArrayCache;
use Zend\Cache\StorageFactory;

/**
 * @covers Guzzle\Cache\CacheAdapterFactory
 */
class CacheAdapterFactoryTest extends \Guzzle\Tests\GuzzleTestCase
{
    /** @var ArrayCache */
    private $cache;

    /** @var DoctrineCacheAdapter */
    private $adapter;

    /**
     * Prepares the environment before running a test.
     */
    protected function setup()
    {
        parent::setUp();
        $this->cache = new ArrayCache();
        $this->adapter = new DoctrineCacheAdapter($this->cache);
    }

    /**
     * @expectedException \InvalidArgumentException
     */
    public function testEnsuresConfigIsObject()
    {
        CacheAdapterFactory::fromCache(array());
    }

    /**
     * @expectedException \InvalidArgumentException
     */
    public function testEnsuresKnownType()
    {
        CacheAdapterFactory::fromCache(new \stdClass());
    }

    public function cacheProvider()
    {
        return array(
            array(new DoctrineCacheAdapter(new ArrayCache()), 'Guzzle\Cache\DoctrineCacheAdapter'),
            array(new ArrayCache(), 'Guzzle\Cache\DoctrineCacheAdapter'),
            array(StorageFactory::factory(array('adapter' => 'memory')), 'Guzzle\Cache\Zf2CacheAdapter'),
        );
    }

    /**
     * @dataProvider cacheProvider
     */
    public function testCreatesNullCacheAdapterByDefault($cache, $type)
    {
        $adapter = CacheAdapterFactory::fromCache($cache);
        $this->assertInstanceOf($type, $adapter);
    }
}
