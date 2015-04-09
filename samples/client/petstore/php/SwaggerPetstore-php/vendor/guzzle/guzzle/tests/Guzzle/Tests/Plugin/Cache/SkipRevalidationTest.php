<?php

namespace Guzzle\Tests\Plugin\Cache;

use Guzzle\Http\Message\Request;
use Guzzle\Http\Message\Response;
use Guzzle\Plugin\Cache\SkipRevalidation;

/**
 * @covers Guzzle\Plugin\Cache\SkipRevalidation
 */
class SkipRevalidationTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testSkipsRequestRevalidation()
    {
        $skip = new SkipRevalidation();
        $this->assertTrue($skip->revalidate(new Request('GET', 'http://foo.com'), new Response(200)));
    }
}
