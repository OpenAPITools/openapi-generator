<?php

namespace Guzzle\Tests\Stream;

use Guzzle\Stream\Stream;

/**
 * @group server
 * @covers Guzzle\Stream\Stream
 */
class StreamTest extends \Guzzle\Tests\GuzzleTestCase
{
    /**
     * @expectedException \InvalidArgumentException
     */
    public function testConstructorThrowsExceptionOnInvalidArgument()
    {
        $stream = new Stream(true);
    }

    public function testConstructor()
    {
        $handle = fopen('php://temp', 'r+');
        fwrite($handle, 'data');
        $stream = new Stream($handle);
        $this->assertEquals($handle, $stream->getStream());
        $this->assertTrue($stream->isReadable());
        $this->assertTrue($stream->isWritable());
        $this->assertTrue($stream->isLocal());
        $this->assertTrue($stream->isSeekable());
        $this->assertEquals('PHP', $stream->getWrapper());
        $this->assertEquals('TEMP', $stream->getStreamType());
        $this->assertEquals(4, $stream->getSize());
        $this->assertEquals('php://temp', $stream->getUri());
        $this->assertEquals(array(), $stream->getWrapperData());
        $this->assertFalse($stream->isConsumed());
        unset($stream);
    }

    public function testCanModifyStream()
    {
        $handle1 = fopen('php://temp', 'r+');
        $handle2 = fopen('php://temp', 'r+');
        $stream = new Stream($handle1);
        $this->assertSame($handle1, $stream->getStream());
        $stream->setStream($handle2, 10);
        $this->assertEquals(10, $stream->getSize());
        $this->assertSame($handle2, $stream->getStream());
    }

    public function testStreamClosesHandleOnDestruct()
    {
        $handle = fopen('php://temp', 'r');
        $stream = new Stream($handle);
        unset($stream);
        $this->assertFalse(is_resource($handle));
    }

    public function testConvertsToString()
    {
        $handle = fopen('php://temp', 'w+');
        fwrite($handle, 'data');
        $stream = new Stream($handle);
        $this->assertEquals('data', (string) $stream);
        unset($stream);

        $handle = fopen(__DIR__ . '/../TestData/FileBody.txt', 'r');
        $stream = new Stream($handle);
        $this->assertEquals('', (string) $stream);
        unset($stream);
    }

    public function testConvertsToStringAndRestoresCursorPos()
    {
        $handle = fopen('php://temp', 'w+');
        $stream = new Stream($handle);
        $stream->write('foobazbar');
        $stream->seek(3);
        $this->assertEquals('foobazbar', (string) $stream);
        $this->assertEquals(3, $stream->ftell());
    }

    public function testIsConsumed()
    {
        $handle = fopen('php://temp', 'w+');
        fwrite($handle, 'data');
        $stream = new Stream($handle);
        $this->assertFalse($stream->isConsumed());
        $stream->read(4);
        $this->assertTrue($stream->isConsumed());
    }

    public function testAllowsSettingManualSize()
    {
        $handle = fopen('php://temp', 'w+');
        fwrite($handle, 'data');
        $stream = new Stream($handle);
        $stream->setSize(10);
        $this->assertEquals(10, $stream->getSize());
        unset($stream);
    }

    public function testWrapsStream()
    {
        $handle = fopen('php://temp', 'w+');
        fwrite($handle, 'data');
        $stream = new Stream($handle);
        $this->assertTrue($stream->isSeekable());
        $this->assertTrue($stream->isReadable());
        $this->assertTrue($stream->seek(0));
        $this->assertEquals('da', $stream->read(2));
        $this->assertEquals('ta', $stream->read(2));
        $this->assertTrue($stream->seek(0));
        $this->assertEquals('data', $stream->read(4));
        $stream->write('_appended');
        $stream->seek(0);
        $this->assertEquals('data_appended', $stream->read(13));
    }

    public function testGetSize()
    {
        $size = filesize(__DIR__ . '/../../../bootstrap.php');
        $handle = fopen(__DIR__ . '/../../../bootstrap.php', 'r');
        $stream = new Stream($handle);
        $this->assertEquals($handle, $stream->getStream());
        $this->assertEquals($size, $stream->getSize());
        $this->assertEquals($size, $stream->getSize());
        unset($stream);

        // Make sure that false is returned when the size cannot be determined
        $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n");
        $handle = fopen('http://127.0.0.1:' . $this->getServer()->getPort(), 'r');
        $stream = new Stream($handle);
        $this->assertEquals(false, $stream->getSize());
        unset($stream);
    }

    public function testEnsuresSizeIsConsistent()
    {
        $h = fopen('php://temp', 'r+');
        fwrite($h, 'foo');
        $stream = new Stream($h);
        $this->assertEquals(3, $stream->getSize());
        $stream->write('test');
        $this->assertEquals(7, $stream->getSize());
        fclose($h);
    }

    public function testAbstractsMetaData()
    {
        $handle = fopen(__DIR__ . '/../../../bootstrap.php', 'r');
        $stream = new Stream($handle);
        $this->assertEquals('plainfile', $stream->getMetaData('wrapper_type'));
        $this->assertEquals(null, $stream->getMetaData('wrapper_data'));
        $this->assertInternalType('array', $stream->getMetaData());
    }

    public function testDoesNotAttemptToWriteToReadonlyStream()
    {
        $handle = fopen(__DIR__ . '/../../../bootstrap.php', 'r');
        $stream = new Stream($handle);
        $this->assertEquals(0, $stream->write('foo'));
    }

    public function testProvidesStreamPosition()
    {
        $handle = fopen(__DIR__ . '/../../../bootstrap.php', 'r');
        $stream = new Stream($handle);
        $stream->read(2);
        $this->assertSame(ftell($handle), $stream->ftell());
        $this->assertEquals(2, $stream->ftell());
    }

    public function testRewindIsSeekZero()
    {
        $stream = new Stream(fopen('php://temp', 'w+'));
        $stream->write('foobazbar');
        $this->assertTrue($stream->rewind());
        $this->assertEquals('foobazbar', $stream->read(9));
    }

    public function testCanDetachStream()
    {
        $r = fopen('php://temp', 'w+');
        $stream = new Stream($r);
        $stream->detachStream();
        $this->assertNull($stream->getStream());
    }
}
