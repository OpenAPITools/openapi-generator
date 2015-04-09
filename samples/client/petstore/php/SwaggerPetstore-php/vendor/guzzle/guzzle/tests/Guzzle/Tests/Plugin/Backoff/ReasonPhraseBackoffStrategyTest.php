<?php

namespace Guzzle\Tests\Plugin\Backoff;

use Guzzle\Plugin\Backoff\ReasonPhraseBackoffStrategy;
use Guzzle\Http\Message\Response;

/**
 * @covers Guzzle\Plugin\Backoff\ReasonPhraseBackoffStrategy
 * @covers Guzzle\Plugin\Backoff\AbstractErrorCodeBackoffStrategy
 */
class ReasonPhraseBackoffStrategyTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testRetriesWhenCodeMatches()
    {
        $this->assertEmpty(ReasonPhraseBackoffStrategy::getDefaultFailureCodes());
        $strategy = new ReasonPhraseBackoffStrategy(array('Foo', 'Internal Server Error'));
        $this->assertTrue($strategy->makesDecision());
        $request = $this->getMock('Guzzle\Http\Message\Request', array(), array(), '', false);
        $response = new Response(200);
        $this->assertEquals(false, $strategy->getBackoffPeriod(0, $request, $response));
        $response->setStatus(200, 'Foo');
        $this->assertEquals(0, $strategy->getBackoffPeriod(0, $request, $response));
    }

    public function testIgnoresNonErrors()
    {
        $strategy = new ReasonPhraseBackoffStrategy();
        $request = $this->getMock('Guzzle\Http\Message\Request', array(), array(), '', false);
        $this->assertEquals(false, $strategy->getBackoffPeriod(0, $request));
    }
}
