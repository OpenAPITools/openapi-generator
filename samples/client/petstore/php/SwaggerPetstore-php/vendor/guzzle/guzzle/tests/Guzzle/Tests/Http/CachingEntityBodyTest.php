<?php

namespace Guzzle\Tests\Http;

use Guzzle\Http\EntityBody;
use Guzzle\Http\CachingEntityBody;

/**
 * @covers Guzzle\Http\CachingEntityBody
 */
class CachingEntityBodyTest extends \Guzzle\Tests\GuzzleTestCase
{
    /** @var CachingEntityBody */
    protected $body;

    /** @var EntityBody */
    protected $decorated;

    public function setUp()
    {
        $this->decorated = EntityBody::factory('testing');
        $this->body = new CachingEntityBody($this->decorated);
    }

    public function testUsesRemoteSizeIfPossible()
    {
        $body = EntityBody::factory('test');
        $caching = new CachingEntityBody($body);
        $this->assertEquals(4, $caching->getSize());
        $this->assertEquals(4, $caching->getContentLength());
    }

    /**
     * @expectedException \Guzzle\Common\Exception\RuntimeException
     * @expectedExceptionMessage does not support custom stream rewind
     */
    public function testDoesNotAllowRewindFunction()
    {
        $this->body->setRewindFunction(true);
    }

    /**
     * @expectedException \Guzzle\Common\Exception\RuntimeException
     * @expectedExceptionMessage Cannot seek to byte 10
     */
    public function testCannotSeekPastWhatHasBeenRead()
    {
        $this->body->seek(10);
    }

    /**
     * @expectedException \Guzzle\Common\Exception\RuntimeException
     * @expectedExceptionMessage supports only SEEK_SET and SEEK_CUR
     */
    public function testCannotUseSeekEnd()
    {
        $this->body->seek(2, SEEK_END);
    }

    public function testChangingUnderlyingStreamUpdatesSizeAndStream()
    {
        $size = filesize(__FILE__);
        $s = fopen(__FILE__, 'r');
        $this->body->setStream($s, $size);
        $this->assertEquals($size, $this->body->getSize());
        $this->assertEquals($size, $this->decorated->getSize());
        $this->assertSame($s, $this->body->getStream());
        $this->assertSame($s, $this->decorated->getStream());
    }

    public function testRewindUsesSeek()
    {
        $a = EntityBody::factory('foo');
        $d = $this->getMockBuilder('Guzzle\Http\CachingEntityBody')
            ->setMethods(array('seek'))
            ->setConstructorArgs(array($a))
            ->getMock();
        $d->expects($this->once())
            ->method('seek')
            ->with(0)
            ->will($this->returnValue(true));
        $d->rewind();
    }

    public function testCanSeekToReadBytes()
    {
        $this->assertEquals('te', $this->body->read(2));
        $this->body->seek(0);
        $this->assertEquals('test', $this->body->read(4));
        $this->assertEquals(4, $this->body->ftell());
        $this->body->seek(2);
        $this->assertEquals(2, $this->body->ftell());
        $this->body->seek(2, SEEK_CUR);
        $this->assertEquals(4, $this->body->ftell());
        $this->assertEquals('ing', $this->body->read(3));
    }

    public function testWritesToBufferStream()
    {
        $this->body->read(2);
        $this->body->write('hi');
        $this->body->rewind();
        $this->assertEquals('tehiing', (string) $this->body);
    }

    public function testReadLinesFromBothStreams()
    {
        $this->body->seek($this->body->ftell());
        $this->body->write("test\n123\nhello\n1234567890\n");
        $this->body->rewind();
        $this->assertEquals("test\n", $this->body->readLine(7));
        $this->assertEquals("123\n", $this->body->readLine(7));
        $this->assertEquals("hello\n", $this->body->readLine(7));
        $this->assertEquals("123456", $this->body->readLine(7));
        $this->assertEquals("7890\n", $this->body->readLine(7));
        // We overwrote the decorated stream, so no more data
        $this->assertEquals('', $this->body->readLine(7));
    }

    public function testSkipsOverwrittenBytes()
    {
        $decorated = EntityBody::factory(
            implode("\n", array_map(function ($n) {
                return str_pad($n, 4, '0', STR_PAD_LEFT);
            }, range(0, 25)))
        );

        $body = new CachingEntityBody($decorated);

        $this->assertEquals("0000\n", $body->readLine());
        $this->assertEquals("0001\n", $body->readLine());
        // Write over part of the body yet to be read, so skip some bytes
        $this->assertEquals(5, $body->write("TEST\n"));
        $this->assertEquals(5, $this->readAttribute($body, 'skipReadBytes'));
        // Read, which skips bytes, then reads
        $this->assertEquals("0003\n", $body->readLine());
        $this->assertEquals(0, $this->readAttribute($body, 'skipReadBytes'));
        $this->assertEquals("0004\n", $body->readLine());
        $this->assertEquals("0005\n", $body->readLine());

        // Overwrite part of the cached body (so don't skip any bytes)
        $body->seek(5);
        $this->assertEquals(5, $body->write("ABCD\n"));
        $this->assertEquals(0, $this->readAttribute($body, 'skipReadBytes'));
        $this->assertEquals("TEST\n", $body->readLine());
        $this->assertEquals("0003\n", $body->readLine());
        $this->assertEquals("0004\n", $body->readLine());
        $this->assertEquals("0005\n", $body->readLine());
        $this->assertEquals("0006\n", $body->readLine());
        $this->assertEquals(5, $body->write("1234\n"));
        $this->assertEquals(5, $this->readAttribute($body, 'skipReadBytes'));

        // Seek to 0 and ensure the overwritten bit is replaced
        $body->rewind();
        $this->assertEquals("0000\nABCD\nTEST\n0003\n0004\n0005\n0006\n1234\n0008\n0009\n", $body->read(50));

        // Ensure that casting it to a string does not include the bit that was overwritten
        $this->assertContains("0000\nABCD\nTEST\n0003\n0004\n0005\n0006\n1234\n0008\n0009\n", (string) $body);
    }

    public function testWrapsContentType()
    {
        $a = $this->getMockBuilder('Guzzle\Http\EntityBody')
            ->setMethods(array('getContentType'))
            ->setConstructorArgs(array(fopen(__FILE__, 'r')))
            ->getMock();
        $a->expects($this->once())
            ->method('getContentType')
            ->will($this->returnValue('foo'));
        $d = new CachingEntityBody($a);
        $this->assertEquals('foo', $d->getContentType());
    }

    public function testWrapsContentEncoding()
    {
        $a = $this->getMockBuilder('Guzzle\Http\EntityBody')
            ->setMethods(array('getContentEncoding'))
            ->setConstructorArgs(array(fopen(__FILE__, 'r')))
            ->getMock();
        $a->expects($this->once())
            ->method('getContentEncoding')
            ->will($this->returnValue('foo'));
        $d = new CachingEntityBody($a);
        $this->assertEquals('foo', $d->getContentEncoding());
    }

    public function testWrapsMetadata()
    {
        $a = $this->getMockBuilder('Guzzle\Http\EntityBody')
            ->setMethods(array('getMetadata', 'getWrapper', 'getWrapperData', 'getStreamType', 'getUri'))
            ->setConstructorArgs(array(fopen(__FILE__, 'r')))
            ->getMock();

        $a->expects($this->once())
            ->method('getMetadata')
            ->will($this->returnValue(array()));
        // Called twice for getWrapper and getWrapperData
        $a->expects($this->exactly(1))
            ->method('getWrapper')
            ->will($this->returnValue('wrapper'));
        $a->expects($this->once())
            ->method('getWrapperData')
            ->will($this->returnValue(array()));
        $a->expects($this->once())
            ->method('getStreamType')
            ->will($this->returnValue('baz'));
        $a->expects($this->once())
            ->method('getUri')
            ->will($this->returnValue('path/to/foo'));

        $d = new CachingEntityBody($a);
        $this->assertEquals(array(), $d->getMetaData());
        $this->assertEquals('wrapper', $d->getWrapper());
        $this->assertEquals(array(), $d->getWrapperData());
        $this->assertEquals('baz', $d->getStreamType());
        $this->assertEquals('path/to/foo', $d->getUri());
    }

    public function testWrapsCustomData()
    {
        $a = $this->getMockBuilder('Guzzle\Http\EntityBody')
            ->setMethods(array('getCustomData', 'setCustomData'))
            ->setConstructorArgs(array(fopen(__FILE__, 'r')))
            ->getMock();

        $a->expects($this->exactly(1))
            ->method('getCustomData')
            ->with('foo')
            ->will($this->returnValue('bar'));

        $a->expects($this->exactly(1))
            ->method('setCustomData')
            ->with('foo', 'bar')
            ->will($this->returnSelf());

        $d = new CachingEntityBody($a);
        $this->assertSame($d, $d->setCustomData('foo', 'bar'));
        $this->assertEquals('bar', $d->getCustomData('foo'));
    }

    public function testClosesBothStreams()
    {
        $s = fopen('php://temp', 'r');
        $a = EntityBody::factory($s);
        $d = new CachingEntityBody($a);
        $d->close();
        $this->assertFalse(is_resource($s));
    }
}
