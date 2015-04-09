<?php

namespace Guzzle\Tests\Batch;

use Guzzle\Batch\HistoryBatch;
use Guzzle\Batch\Batch;

/**
 * @covers Guzzle\Batch\HistoryBatch
 */
class HistoryBatchTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testMaintainsHistoryOfItemsAddedToBatch()
    {
        $batch = new Batch(
            $this->getMock('Guzzle\Batch\BatchTransferInterface'),
            $this->getMock('Guzzle\Batch\BatchDivisorInterface')
        );

        $history = new HistoryBatch($batch);
        $history->add('foo')->add('baz');
        $this->assertEquals(array('foo', 'baz'), $history->getHistory());
        $history->clearHistory();
        $this->assertEquals(array(), $history->getHistory());
    }
}
