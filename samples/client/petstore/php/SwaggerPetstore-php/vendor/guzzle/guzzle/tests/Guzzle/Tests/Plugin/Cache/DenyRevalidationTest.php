<?php

namespace Guzzle\Tests\Plugin\Cache;

use Guzzle\Http\Message\Request;
use Guzzle\Http\Message\Response;
use Guzzle\Plugin\Cache\DenyRevalidation;

/**
 * @covers Guzzle\Plugin\Cache\DenyRevalidation
 */
class DenyRevalidationTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testDeniesRequestRevalidation()
    {
        $deny = new DenyRevalidation();
        $this->assertFalse($deny->revalidate(new Request('GET', 'http://foo.com'), new Response(200)));
    }
}
