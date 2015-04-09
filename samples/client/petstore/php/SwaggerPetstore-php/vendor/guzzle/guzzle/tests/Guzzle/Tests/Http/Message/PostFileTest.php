<?php

namespace Guzzle\Tests\Http\Message;

use Guzzle\Http\Client;
use Guzzle\Http\Message\PostFile;

/**
 * @covers Guzzle\Http\Message\PostFile
 * @group server
 */
class PostFileTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testConstructorConfiguresPostFile()
    {
        $file = new PostFile('foo', __FILE__, 'x-foo', 'boo');
        $this->assertEquals('foo', $file->getFieldName());
        $this->assertEquals(__FILE__, $file->getFilename());
        $this->assertEquals('boo', $file->getPostName());
        $this->assertEquals('x-foo', $file->getContentType());
    }

    public function testRemovesLeadingAtSymbolFromPath()
    {
        $file = new PostFile('foo', '@' . __FILE__);
        $this->assertEquals(__FILE__, $file->getFilename());
    }

    /**
     * @expectedException Guzzle\Common\Exception\InvalidArgumentException
     */
    public function testEnsuresFileIsReadable()
    {
        $file = new PostFile('foo', '/foo/baz/bar');
    }

    public function testCanChangeContentType()
    {
        $file = new PostFile('foo', '@' . __FILE__);
        $file->setContentType('Boo');
        $this->assertEquals('Boo', $file->getContentType());
    }

    public function testCanChangeFieldName()
    {
        $file = new PostFile('foo', '@' . __FILE__);
        $file->setFieldName('Boo');
        $this->assertEquals('Boo', $file->getFieldName());
    }

    public function testReturnsCurlValueString()
    {
        $file = new PostFile('foo', __FILE__);
        if (version_compare(phpversion(), '5.5.0', '<')) {
            $this->assertContains('@' . __FILE__ . ';filename=PostFileTest.php;type=text/x-', $file->getCurlValue());
        } else {
            $c = $file->getCurlValue();
            $this->assertEquals(__FILE__, $c->getFilename());
            $this->assertEquals('PostFileTest.php', $c->getPostFilename());
            $this->assertContains('text/x-', $c->getMimeType());
        }
    }

    public function testReturnsCurlValueStringAndPostname()
    {
        $file = new PostFile('foo', __FILE__, null, 'NewPostFileTest.php');
        if (version_compare(phpversion(), '5.5.0', '<')) {
            $this->assertContains('@' . __FILE__ . ';filename=NewPostFileTest.php;type=text/x-', $file->getCurlValue());
        } else {
            $c = $file->getCurlValue();
            $this->assertEquals(__FILE__, $c->getFilename());
            $this->assertEquals('NewPostFileTest.php', $c->getPostFilename());
            $this->assertContains('text/x-', $c->getMimeType());
        }
    }

    public function testContentDispositionFilePathIsStripped()
    {
        $this->getServer()->flush();
        $client = new Client($this->getServer()->getUrl());
        $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n");
        $request = $client->post()->addPostFile('file', __FILE__);
        $request->send();
        $requests = $this->getServer()->getReceivedRequests(false);
        $this->assertContains('POST / HTTP/1.1', $requests[0]);
        $this->assertContains('Content-Disposition: form-data; name="file"; filename="PostFileTest.php"', $requests[0]);
    }
}
