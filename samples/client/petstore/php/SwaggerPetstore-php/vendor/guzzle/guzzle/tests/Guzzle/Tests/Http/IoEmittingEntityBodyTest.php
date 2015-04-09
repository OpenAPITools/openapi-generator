<?php

namespace Guzzle\Tests\Http;

use Guzzle\Http\EntityBody;
use Guzzle\Http\IoEmittingEntityBody;

/**
 * @covers Guzzle\Http\IoEmittingEntityBody
 */
class IoEmittingEntityBodyTest extends \Guzzle\Tests\GuzzleTestCase
{
    protected $body;
    protected $decorated;

    public function setUp()
    {
        $this->decorated = EntityBody::factory('hello');
        $this->body = new IoEmittingEntityBody($this->decorated);
    }

    public function testEmitsReadEvents()
    {
        $e = null;
        $this->body->getEventDispatcher()->addListener('body.read', function ($event) use (&$e) {
            $e = $event;
        });
        $this->assertEquals('hel', $this->body->read(3));
        $this->assertEquals('hel', $e['read']);
        $this->assertEquals(3, $e['length']);
        $this->assertSame($this->body, $e['body']);
    }

    public function testEmitsWriteEvents()
    {
        $e = null;
        $this->body->getEventDispatcher()->addListener('body.write', function ($event) use (&$e) {
            $e = $event;
        });
        $this->body->seek(0, SEEK_END);
        $this->assertEquals(5, $this->body->write('there'));
        $this->assertEquals('there', $e['write']);
        $this->assertEquals(5, $e['result']);
        $this->assertSame($this->body, $e['body']);
        $this->assertEquals('hellothere', (string) $this->body);
    }
}
