<?php

namespace Guzzle\Tests\Http\Message;

use Guzzle\Http\Client;
use Guzzle\Http\EntityBody;
use Guzzle\Http\Message\Request;
use Guzzle\Http\Message\RequestFactory;
use Guzzle\Http\RedirectPlugin;
use Guzzle\Http\Message\EntityEnclosingRequest;
use Guzzle\Http\Message\PostFile;
use Guzzle\Http\QueryString;

/**
 * @group server
 * @covers Guzzle\Http\Message\EntityEnclosingRequest
 */
class EntityEnclosingRequestTest extends \Guzzle\Tests\GuzzleTestCase
{
    protected $client;

    public function setUp()
    {
        $this->client = new Client();
    }

    public function tearDown()
    {
        $this->client = null;
    }

    public function testConstructorConfiguresRequest()
    {
        $request = new EntityEnclosingRequest('PUT', 'http://test.com', array(
            'X-Test' => '123'
        ));
        $request->setBody('Test');
        $this->assertEquals('123', $request->getHeader('X-Test'));
        $this->assertNull($request->getHeader('Expect'));
    }

    public function testCanSetBodyWithoutOverridingContentType()
    {
        $request = new EntityEnclosingRequest('PUT', 'http://test.com', array('Content-Type' => 'foooooo'));
        $request->setBody('{"a":"b"}');
        $this->assertEquals('foooooo', $request->getHeader('Content-Type'));
    }

    public function testRequestIncludesBodyInMessage()
    {

        $request = RequestFactory::getInstance()->create('PUT', 'http://www.guzzle-project.com/', null, 'data');
        $this->assertEquals("PUT / HTTP/1.1\r\n"
            . "Host: www.guzzle-project.com\r\n"
            . "Content-Length: 4\r\n\r\n"
            . "data", (string) $request);
    }

    public function testRequestIncludesPostBodyInMessageOnlyWhenNoPostFiles()
    {
        $request = RequestFactory::getInstance()->create('POST', 'http://www.guzzle-project.com/', null, array(
            'foo' => 'bar'
        ));
        $this->assertEquals("POST / HTTP/1.1\r\n"
            . "Host: www.guzzle-project.com\r\n"
            . "Content-Type: application/x-www-form-urlencoded; charset=utf-8\r\n\r\n"
            . "foo=bar", (string) $request);

        $request = RequestFactory::getInstance()->create('POST', 'http://www.guzzle-project.com/', null, array(
            'foo' => '@' . __FILE__
        ));
        $this->assertEquals("POST / HTTP/1.1\r\n"
            . "Host: www.guzzle-project.com\r\n"
            . "Content-Type: multipart/form-data\r\n"
            . "Expect: 100-Continue\r\n\r\n", (string) $request);
    }

    public function testAddsPostFieldsAndSetsContentLength()
    {
        $request = RequestFactory::getInstance()->create('POST', 'http://www.guzzle-project.com/', null, array(
            'data' => '123'
        ));
        $this->assertEquals("POST / HTTP/1.1\r\n"
            . "Host: www.guzzle-project.com\r\n"
            . "Content-Type: application/x-www-form-urlencoded; charset=utf-8\r\n\r\n"
            . "data=123", (string) $request);
    }

    public function testAddsPostFilesAndSetsContentType()
    {
        $request = RequestFactory::getInstance()->create('POST', 'http://www.test.com/')
            ->addPostFiles(array(
                'file' => __FILE__
            ))->addPostFields(array(
                'a' => 'b'
            ));
        $message = (string) $request;
        $this->assertEquals('multipart/form-data', $request->getHeader('Content-Type'));
        $this->assertEquals('100-Continue', $request->getHeader('Expect'));
    }

    public function testRequestBodyContainsPostFiles()
    {
        $request = RequestFactory::getInstance()->create('POST', 'http://www.test.com/');
        $request->addPostFields(array(
            'test' => '123'
        ));
        $this->assertContains("\r\n\r\ntest=123", (string) $request);
    }

    public function testRequestBodyAddsContentLength()
    {
        $request = RequestFactory::getInstance()->create('PUT', 'http://www.test.com/');
        $request->setBody(EntityBody::factory('test'));
        $this->assertEquals(4, (string) $request->getHeader('Content-Length'));
        $this->assertFalse($request->hasHeader('Transfer-Encoding'));
    }

    public function testRequestBodyDoesNotUseContentLengthWhenChunked()
    {
        $request = RequestFactory::getInstance()->create('PUT', 'http://www.test.com/', array(
            'Transfer-Encoding' => 'chunked'
        ), 'test');
        $this->assertNull($request->getHeader('Content-Length'));
        $this->assertTrue($request->hasHeader('Transfer-Encoding'));
    }

    public function testRequestHasMutableBody()
    {
        $request = RequestFactory::getInstance()->create('PUT', 'http://www.guzzle-project.com/', null, 'data');
        $body = $request->getBody();
        $this->assertInstanceOf('Guzzle\\Http\\EntityBody', $body);
        $this->assertSame($body, $request->getBody());

        $newBody = EntityBody::factory('foobar');
        $request->setBody($newBody);
        $this->assertEquals('foobar', (string) $request->getBody());
        $this->assertSame($newBody, $request->getBody());
    }

    public function testSetPostFields()
    {
        $request = RequestFactory::getInstance()->create('POST', 'http://www.guzzle-project.com/');
        $this->assertInstanceOf('Guzzle\\Http\\QueryString', $request->getPostFields());

        $fields = new QueryString(array(
            'a' => 'b'
        ));
        $request->addPostFields($fields);
        $this->assertEquals($fields->getAll(), $request->getPostFields()->getAll());
        $this->assertEquals(array(), $request->getPostFiles());
    }

    public function testSetPostFiles()
    {
        $request = RequestFactory::getInstance()->create('POST', $this->getServer()->getUrl())
            ->setClient(new Client())
            ->addPostFiles(array(__FILE__))
            ->addPostFields(array(
                'test' => 'abc'
            ));

        $request->getCurlOptions()->set('debug', true);

        $this->assertEquals(array(
            'test' => 'abc'
        ), $request->getPostFields()->getAll());

        $files = $request->getPostFiles();
        $post = $files['file'][0];
        $this->assertEquals('file', $post->getFieldName());
        $this->assertContains('text/x-', $post->getContentType());
        $this->assertEquals(__FILE__, $post->getFilename());

        $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n");
        $request->send();

        $this->assertNotNull($request->getHeader('Content-Length'));
        $this->assertContains('multipart/form-data; boundary=', (string) $request->getHeader('Content-Type'), '-> cURL must add the boundary');
    }

    /**
     * @expectedException Guzzle\Common\Exception\InvalidArgumentException
     */
    public function testSetPostFilesThrowsExceptionWhenFileIsNotFound()
    {
        $request = RequestFactory::getInstance()->create('POST', 'http://www.guzzle-project.com/')
            ->addPostFiles(array(
                'file' => 'filenotfound.ini'
            ));
    }

    /**
     * @expectedException Guzzle\Http\Exception\RequestException
     */
    public function testThrowsExceptionWhenNonStringsAreAddedToPost()
    {
        $request = RequestFactory::getInstance()->create('POST', 'http://www.guzzle-project.com/')
            ->addPostFile('foo', new \stdClass());
    }

    public function testAllowsContentTypeInPostUploads()
    {
        $request = RequestFactory::getInstance()->create('POST', 'http://www.guzzle-project.com/')
            ->addPostFile('foo', __FILE__, 'text/plain');

        $this->assertEquals(array(
            new PostFile('foo', __FILE__, 'text/plain')
        ), $request->getPostFile('foo'));
    }

    public function testGuessesContentTypeOfPostUpload()
    {
        $request = RequestFactory::getInstance()->create('POST', 'http://www.guzzle-project.com/')
            ->addPostFile('foo', __FILE__);
        $file = $request->getPostFile('foo');
        $this->assertContains('text/x-', $file[0]->getContentType());
    }

    public function testAllowsContentDispositionFieldsInPostUploadsWhenSettingInBulk()
    {
        $postFile = new PostFile('foo', __FILE__, 'text/x-php');
        $request = RequestFactory::getInstance()->create('POST', 'http://www.guzzle-project.com/')
            ->addPostFiles(array('foo' => $postFile));

        $this->assertEquals(array($postFile), $request->getPostFile('foo'));
    }

    public function testPostRequestsUseApplicationXwwwForUrlEncodedForArrays()
    {
        $request = RequestFactory::getInstance()->create('POST', 'http://www.guzzle-project.com/');
        $request->setPostField('a', 'b');
        $this->assertContains("\r\n\r\na=b", (string) $request);
        $this->assertEquals('application/x-www-form-urlencoded; charset=utf-8', $request->getHeader('Content-Type'));
    }

    public function testProcessMethodAddsContentType()
    {
        $request = RequestFactory::getInstance()->create('POST', 'http://www.guzzle-project.com/');
        $request->setPostField('a', 'b');
        $this->assertEquals('application/x-www-form-urlencoded; charset=utf-8', $request->getHeader('Content-Type'));
    }

    public function testPostRequestsUseMultipartFormDataWithFiles()
    {
        $request = RequestFactory::getInstance()->create('POST', 'http://www.guzzle-project.com/');
        $request->addPostFiles(array('file' => __FILE__));
        $this->assertEquals('multipart/form-data', $request->getHeader('Content-Type'));
    }

    public function testCanSendMultipleRequestsUsingASingleRequestObject()
    {
        $this->getServer()->flush();
        $this->getServer()->enqueue(array(
            "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n",
            "HTTP/1.1 201 Created\r\nContent-Length: 0\r\n\r\n",
        ));

        // Send the first request
        $request = RequestFactory::getInstance()->create('PUT', $this->getServer()->getUrl())
            ->setBody('test')
            ->setClient(new Client());
        $request->send();
        $this->assertEquals(200, $request->getResponse()->getStatusCode());

        // Send the second request
        $request->setBody('abcdefg', 'application/json', false);
        $request->send();
        $this->assertEquals(201, $request->getResponse()->getStatusCode());

        // Ensure that the same request was sent twice with different bodies
        $requests = $this->getServer()->getReceivedRequests(true);
        $this->assertEquals(2, count($requests));
        $this->assertEquals(4, (string) $requests[0]->getHeader('Content-Length'));
        $this->assertEquals(7, (string) $requests[1]->getHeader('Content-Length'));
    }

    public function testRemovingPostFieldRebuildsPostFields()
    {
        $request = new EntityEnclosingRequest('POST', 'http://test.com');
        $request->setPostField('test', 'value');
        $request->removePostField('test');
        $this->assertNull($request->getPostField('test'));
    }

    public function testUsesChunkedTransferWhenBodyLengthCannotBeDetermined()
    {
        $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n");
        $request = new EntityEnclosingRequest('PUT', 'http://test.com/');
        $request->setBody(fopen($this->getServer()->getUrl(), 'r'));
        $this->assertEquals('chunked', $request->getHeader('Transfer-Encoding'));
        $this->assertFalse($request->hasHeader('Content-Length'));
    }

    /**
     * @expectedException \Guzzle\Http\Exception\RequestException
     */
    public function testThrowsExceptionWhenContentLengthCannotBeDeterminedAndUsingHttp1()
    {
        $request = new EntityEnclosingRequest('PUT', 'http://test.com/');
        $this->getServer()->enqueue("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n");
        $request->setProtocolVersion('1.0');
        $request->setBody(fopen($this->getServer()->getUrl(), 'r'));
    }

    public function testAllowsNestedPostData()
    {
        $request = new EntityEnclosingRequest('POST', 'http://test.com/');
        $request->addPostFields(array(
            'a' => array('b', 'c')
        ));
        $this->assertEquals(array(
            'a' => array('b', 'c')
        ), $request->getPostFields()->getAll());
    }

    public function testAllowsEmptyFields()
    {
        $request = new EntityEnclosingRequest('POST', 'http://test.com/');
        $request->addPostFields(array(
            'a' => ''
        ));
        $this->assertEquals(array(
            'a' => ''
        ), $request->getPostFields()->getAll());
    }

    /**
     * @expectedException \Guzzle\Http\Exception\RequestException
     */
    public function testFailsOnInvalidFiles()
    {
        $request = new EntityEnclosingRequest('POST', 'http://test.com/');
        $request->addPostFiles(array(
            'a' => new \stdClass()
        ));
    }

    public function testHandlesEmptyStrings()
    {
        $request = new EntityEnclosingRequest('POST', 'http://test.com/');
        $request->addPostFields(array(
            'a' => '',
            'b' => null,
            'c' => 'Foo'
        ));
        $this->assertEquals(array(
            'a' => '',
            'b' => null,
            'c' => 'Foo'
        ), $request->getPostFields()->getAll());
    }

    public function testHoldsPostFiles()
    {
        $request = new EntityEnclosingRequest('POST', 'http://test.com/');
        $request->addPostFile('foo', __FILE__);
        $request->addPostFile(new PostFile('foo', __FILE__));

        $this->assertArrayHasKey('foo', $request->getPostFiles());
        $foo = $request->getPostFile('foo');
        $this->assertEquals(2, count($foo));
        $this->assertEquals(__FILE__, $foo[0]->getFilename());
        $this->assertEquals(__FILE__, $foo[1]->getFilename());

        $request->removePostFile('foo');
        $this->assertEquals(array(), $request->getPostFiles());
    }

    public function testAllowsAtPrefixWhenAddingPostFiles()
    {
        $request = new EntityEnclosingRequest('POST', 'http://test.com/');
        $request->addPostFiles(array(
            'foo' => '@' . __FILE__
        ));
        $foo = $request->getPostFile('foo');
        $this->assertEquals(__FILE__, $foo[0]->getFilename());
    }

    public function testSetStateToTransferWithEmptyBodySetsContentLengthToZero()
    {
        $request = new EntityEnclosingRequest('POST', 'http://test.com/');
        $request->setState($request::STATE_TRANSFER);
        $this->assertEquals('0', (string) $request->getHeader('Content-Length'));
    }

    public function testSettingExpectHeaderCutoffChangesRequest()
    {
        $request = new EntityEnclosingRequest('PUT', 'http://test.com/');
        $request->setHeader('Expect', '100-Continue');
        $request->setExpectHeaderCutoff(false);
        $this->assertNull($request->getHeader('Expect'));
        // There is not body, so remove the expect header
        $request->setHeader('Expect', '100-Continue');
        $request->setExpectHeaderCutoff(10);
        $this->assertNull($request->getHeader('Expect'));
        // The size is less than the cutoff
        $request->setBody('foo');
        $this->assertNull($request->getHeader('Expect'));
        // The size is greater than the cutoff
        $request->setBody('foobazbarbamboo');
        $this->assertNotNull($request->getHeader('Expect'));
    }

    public function testStrictRedirectsCanBeSpecifiedOnEntityEnclosingRequests()
    {
        $request = new EntityEnclosingRequest('PUT', 'http://test.com/');
        $request->configureRedirects(true);
        $this->assertTrue($request->getParams()->get(RedirectPlugin::STRICT_REDIRECTS));
    }

    public function testCanDisableRedirects()
    {
        $request = new EntityEnclosingRequest('PUT', 'http://test.com/');
        $request->configureRedirects(false, false);
        $this->assertTrue($request->getParams()->get(RedirectPlugin::DISABLE));
    }

    public function testSetsContentTypeWhenSettingBodyByGuessingFromEntityBody()
    {
        $request = new EntityEnclosingRequest('PUT', 'http://test.com/foo');
        $request->setBody(EntityBody::factory(fopen(__FILE__, 'r')));
        $this->assertEquals('text/x-php', (string) $request->getHeader('Content-Type'));
    }

    public function testDoesNotCloneBody()
    {
        $request = new EntityEnclosingRequest('PUT', 'http://test.com/foo');
        $request->setBody('test');
        $newRequest = clone $request;
        $newRequest->setBody('foo');
        $this->assertInternalType('string', (string) $request->getBody());
    }
}
