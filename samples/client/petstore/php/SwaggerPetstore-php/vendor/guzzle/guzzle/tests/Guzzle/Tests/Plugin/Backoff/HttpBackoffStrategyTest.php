<?php

namespace Guzzle\Tests\Plugin\Backoff;

use Guzzle\Plugin\Backoff\HttpBackoffStrategy;
use Guzzle\Http\Message\Response;

/**
 * @covers Guzzle\Plugin\Backoff\HttpBackoffStrategy
 * @covers Guzzle\Plugin\Backoff\AbstractErrorCodeBackoffStrategy
 */
class HttpBackoffStrategyTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testRetriesWhenCodeMatches()
    {
        $this->assertNotEmpty(HttpBackoffStrategy::getDefaultFailureCodes());
        $strategy = new HttpBackoffStrategy();
        $this->assertTrue($strategy->makesDecision());
        $request = $this->getMock('Guzzle\Http\Message\Request', array(), array(), '', false);

        $response = new Response(200);
        $this->assertEquals(false, $strategy->getBackoffPeriod(0, $request, $response));
        $response->setStatus(400);
        $this->assertEquals(false, $strategy->getBackoffPeriod(0, $request, $response));

        foreach (HttpBackoffStrategy::getDefaultFailureCodes() as $code) {
            $this->assertEquals(0, $strategy->getBackoffPeriod(0, $request, $response->setStatus($code)));
        }
    }

    public function testAllowsCustomCodes()
    {
        $strategy = new HttpBackoffStrategy(array(204));
        $request = $this->getMock('Guzzle\Http\Message\Request', array(), array(), '', false);
        $response = new Response(204);
        $this->assertEquals(0, $strategy->getBackoffPeriod(0, $request, $response));
        $response->setStatus(500);
        $this->assertEquals(false, $strategy->getBackoffPeriod(0, $request, $response));
    }

    public function testIgnoresNonErrors()
    {
        $strategy = new HttpBackoffStrategy();
        $request = $this->getMock('Guzzle\Http\Message\Request', array(), array(), '', false);
        $this->assertEquals(false, $strategy->getBackoffPeriod(0, $request));
    }
}
