<?php

namespace Guzzle\Tests\Http\Message;

use Guzzle\Http\Message\Header;
use Guzzle\Http\Message\Request;
use Guzzle\Common\Collection;

/**
 * @covers Guzzle\Http\Message\AbstractMessage
 */
class AbstractMessageTest extends \Guzzle\Tests\GuzzleTestCase
{
    /** @var Request Request object */
    private $request;

    /** @var AbstractMessage */
    private $mock;

    public function setUp()
    {
        parent::setUp();
        $this->mock = $this->getMockForAbstractClass('Guzzle\Http\Message\AbstractMessage');
    }

    public function tearDown()
    {
        $this->mock = $this->request = null;
    }

    public function testGetParams()
    {
        $request = new Request('GET', 'http://example.com');
        $this->assertInstanceOf('Guzzle\\Common\\Collection', $request->getParams());
    }

    public function testAddHeaders()
    {
        $this->mock->setHeader('A', 'B');

        $this->assertEquals($this->mock, $this->mock->addHeaders(array(
            'X-Data' => '123'
        )));

        $this->assertTrue($this->mock->hasHeader('X-Data') !== false);
        $this->assertTrue($this->mock->hasHeader('A') !== false);
    }

    public function testAllowsHeaderToSetAsHeader()
    {
        $h = new Header('A', 'B');
        $this->mock->setHeader('A', $h);
        $this->assertSame($h, $this->mock->getHeader('A'));
    }

    public function testGetHeader()
    {
        $this->mock->setHeader('Test', '123');
        $this->assertEquals('123', $this->mock->getHeader('Test'));
    }

    public function testGetHeaders()
    {
        $this->assertSame($this->mock, $this->mock->setHeaders(array('a' => 'b', 'c' => 'd')));
        $h = $this->mock->getHeaders();
        $this->assertArrayHasKey('a', $h->toArray());
        $this->assertArrayHasKey('c', $h->toArray());
        $this->assertInstanceOf('Guzzle\Http\Message\Header\HeaderInterface', $h->get('a'));
        $this->assertInstanceOf('Guzzle\Http\Message\Header\HeaderInterface', $h->get('c'));
    }

    public function testGetHeaderLinesUsesGlue()
    {
        $this->mock->setHeaders(array('a' => 'b', 'c' => 'd'));
        $this->mock->addHeader('a', 'e');
        $this->mock->getHeader('a')->setGlue('!');
        $this->assertEquals(array(
            'a: b! e',
            'c: d'
        ), $this->mock->getHeaderLines());
    }

    public function testHasHeader()
    {
        $this->assertFalse($this->mock->hasHeader('Foo'));
        $this->mock->setHeader('Foo', 'Bar');
        $this->assertEquals(true, $this->mock->hasHeader('Foo'));
        $this->mock->setHeader('foo', 'yoo');
        $this->assertEquals(true, $this->mock->hasHeader('Foo'));
        $this->assertEquals(true, $this->mock->hasHeader('foo'));
        $this->assertEquals(false, $this->mock->hasHeader('bar'));
    }

    public function testRemoveHeader()
    {
        $this->mock->setHeader('Foo', 'Bar');
        $this->assertEquals(true, $this->mock->hasHeader('Foo'));
        $this->mock->removeHeader('Foo');
        $this->assertFalse($this->mock->hasHeader('Foo'));
    }

    public function testReturnsNullWhenHeaderIsNotFound()
    {
        $this->assertNull($this->mock->getHeader('foo'));
    }

    public function testAddingHeadersPreservesOriginalHeaderCase()
    {
        $this->mock->addHeaders(array(
            'test' => '123',
            'Test' => 'abc'
        ));
        $this->mock->addHeader('test', '456');
        $this->mock->addHeader('test', '789');

        $header = $this->mock->getHeader('test');
        $this->assertContains('123', $header->toArray());
        $this->assertContains('456', $header->toArray());
        $this->assertContains('789', $header->toArray());
        $this->assertContains('abc', $header->toArray());
    }

    public function testCanStoreEmptyHeaders()
    {
        $this->mock->setHeader('Content-Length', 0);
        $this->assertTrue($this->mock->hasHeader('Content-Length'));
        $this->assertEquals(0, (string) $this->mock->getHeader('Content-Length'));
    }

    public function testCanSetCustomHeaderFactory()
    {
        $f = new Header\HeaderFactory();
        $this->mock->setHeaderFactory($f);
        $this->assertSame($f, $this->readAttribute($this->mock, 'headerFactory'));
    }
}
