<?php

namespace Guzzle\Tests\Plugin\Md5;

use Guzzle\Http\EntityBody;
use Guzzle\Http\Message\RequestFactory;
use Guzzle\Http\Message\Response;
use Guzzle\Plugin\Md5\Md5ValidatorPlugin;

/**
 * @covers Guzzle\Plugin\Md5\Md5ValidatorPlugin
 */
class Md5ValidatorPluginTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testValidatesMd5()
    {
        $plugin = new Md5ValidatorPlugin();
        $request = RequestFactory::getInstance()->create('GET', 'http://www.test.com/');
        $request->getEventDispatcher()->addSubscriber($plugin);

        $body = 'abc';
        $hash = md5($body);
        $response = new Response(200, array(
            'Content-MD5' => $hash,
            'Content-Length' => 3
        ), 'abc');

        $request->dispatch('request.complete', array(
            'response' => $response
        ));

        // Try again with no Content-MD5
        $response->removeHeader('Content-MD5');
        $request->dispatch('request.complete', array(
            'response' => $response
        ));
    }

    /**
     * @expectedException UnexpectedValueException
     */
    public function testThrowsExceptionOnInvalidMd5()
    {
        $plugin = new Md5ValidatorPlugin();
        $request = RequestFactory::getInstance()->create('GET', 'http://www.test.com/');
        $request->getEventDispatcher()->addSubscriber($plugin);

        $request->dispatch('request.complete', array(
            'response' => new Response(200, array(
                'Content-MD5' => 'foobar',
                'Content-Length' => 3
            ), 'abc')
        ));
    }

    public function testSkipsWhenContentLengthIsTooLarge()
    {
        $plugin = new Md5ValidatorPlugin(false, 1);
        $request = RequestFactory::getInstance()->create('GET', 'http://www.test.com/');
        $request->getEventDispatcher()->addSubscriber($plugin);

        $request->dispatch('request.complete', array(
            'response' => new Response(200, array(
                'Content-MD5' => 'foobar',
                'Content-Length' => 3
            ), 'abc')
        ));
    }

    public function testProperlyValidatesWhenUsingContentEncoding()
    {
        $plugin = new Md5ValidatorPlugin(true);
        $request = RequestFactory::getInstance()->create('GET', 'http://www.test.com/');
        $request->getEventDispatcher()->addSubscriber($plugin);

        // Content-MD5 is the MD5 hash of the canonical content after all
        // content-encoding has been applied.  Because cURL will automatically
        // decompress entity bodies, we need to re-compress it to calculate.
        $body = EntityBody::factory('abc');
        $body->compress();
        $hash = $body->getContentMd5();
        $body->uncompress();

        $response = new Response(200, array(
            'Content-MD5' => $hash,
            'Content-Encoding' => 'gzip'
        ), 'abc');
        $request->dispatch('request.complete', array(
            'response' => $response
        ));
        $this->assertEquals('abc', $response->getBody(true));

        // Try again with an unknown encoding
        $response = new Response(200, array(
            'Content-MD5' => $hash,
            'Content-Encoding' => 'foobar'
        ), 'abc');
        $request->dispatch('request.complete', array(
            'response' => $response
        ));

        // Try again with compress
        $body->compress('bzip2.compress');
        $response = new Response(200, array(
            'Content-MD5' => $body->getContentMd5(),
            'Content-Encoding' => 'compress'
        ), 'abc');
        $request->dispatch('request.complete', array(
            'response' => $response
        ));

        // Try again with encoding and disabled content-encoding checks
        $request->getEventDispatcher()->removeSubscriber($plugin);
        $plugin = new Md5ValidatorPlugin(false);
        $request->getEventDispatcher()->addSubscriber($plugin);
        $request->dispatch('request.complete', array(
            'response' => $response
        ));
    }
}
