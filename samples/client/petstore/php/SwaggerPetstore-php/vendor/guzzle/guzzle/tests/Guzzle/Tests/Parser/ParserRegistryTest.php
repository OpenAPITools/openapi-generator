<?php

namespace Guzzle\Tests\Parser;

use Guzzle\Parser\ParserRegistry;

/**
 * @covers Guzzle\Parser\ParserRegistry
 */
class ParserRegistryTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testStoresObjects()
    {
        $r = new ParserRegistry();
        $c = new \stdClass();
        $r->registerParser('foo', $c);
        $this->assertSame($c, $r->getParser('foo'));
    }

    public function testReturnsNullWhenNotFound()
    {
        $r = new ParserRegistry();
        $this->assertNull($r->getParser('FOO'));
    }

    public function testReturnsLazyLoadedDefault()
    {
        $r = new ParserRegistry();
        $c = $r->getParser('cookie');
        $this->assertInstanceOf('Guzzle\Parser\Cookie\CookieParser', $c);
        $this->assertSame($c, $r->getParser('cookie'));
    }
}
