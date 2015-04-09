<?php

namespace Guzzle\Tests\Cache;

use Guzzle\Cache\Zf2CacheAdapter;
use Zend\Cache\StorageFactory;

/**
 * @covers Guzzle\Cache\Zf2CacheAdapter
 */
class Zf2CacheAdapterTest extends \Guzzle\Tests\GuzzleTestCase
{
    private $cache;
    private $adapter;

    /**
     * Prepares the environment before running a test.
     */
    protected function setUp()
    {
        parent::setUp();
        $this->cache = StorageFactory::factory(array(
            'adapter' => 'memory'
        ));
        $this->adapter = new Zf2CacheAdapter($this->cache);
    }

    /**
     * Cleans up the environment after running a test.
     */
    protected function tearDown()
    {
        $this->adapter = null;
        $this->cache = null;
        parent::tearDown();
    }

    public function testCachesDataUsingCallables()
    {
        $this->assertTrue($this->adapter->save('test', 'data', 1000));
        $this->assertEquals('data', $this->adapter->fetch('test'));
    }

    public function testChecksIfCacheContainsKeys()
    {
        $this->adapter->save('test', 'data', 1000);
        $this->assertTrue($this->adapter->contains('test'));
        $this->assertFalse($this->adapter->contains('foo'));
    }

    public function testDeletesFromCacheByKey()
    {
        $this->adapter->save('test', 'data', 1000);
        $this->assertTrue($this->adapter->contains('test'));
        $this->adapter->delete('test');
        $this->assertFalse($this->adapter->contains('test'));
    }
}
