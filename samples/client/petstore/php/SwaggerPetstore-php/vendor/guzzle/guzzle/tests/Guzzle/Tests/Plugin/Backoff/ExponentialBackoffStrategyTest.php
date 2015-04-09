<?php

namespace Guzzle\Tests\Plugin\Backoff;

use Guzzle\Plugin\Backoff\ExponentialBackoffStrategy;

/**
 * @covers Guzzle\Plugin\Backoff\ExponentialBackoffStrategy
 */
class ExponentialBackoffStrategyTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testRetriesWithExponentialDelay()
    {
        $strategy = new ExponentialBackoffStrategy();
        $this->assertFalse($strategy->makesDecision());
        $request = $this->getMock('Guzzle\Http\Message\Request', array(), array(), '', false);
        $this->assertEquals(1, $strategy->getBackoffPeriod(0, $request));
        $this->assertEquals(2, $strategy->getBackoffPeriod(1, $request));
        $this->assertEquals(4, $strategy->getBackoffPeriod(2, $request));
        $this->assertEquals(8, $strategy->getBackoffPeriod(3, $request));
        $this->assertEquals(16, $strategy->getBackoffPeriod(4, $request));
    }
}
