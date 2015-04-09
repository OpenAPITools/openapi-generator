<?php

namespace Guzzle\Tests\Batch;

use Guzzle\Batch\FlushingBatch;
use Guzzle\Batch\Batch;

/**
 * @covers Guzzle\Batch\FlushingBatch
 */
class FlushingBatchTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testFlushesWhenSizeMeetsThreshold()
    {
        $t = $this->getMock('Guzzle\Batch\BatchTransferInterface', array('transfer'));
        $d = $this->getMock('Guzzle\Batch\BatchDivisorInterface', array('createBatches'));

        $batch = new Batch($t, $d);
        $queue = $this->readAttribute($batch, 'queue');

        $d->expects($this->exactly(2))
            ->method('createBatches')
            ->will($this->returnCallback(function () use ($queue) {
                $items = array();
                foreach ($queue as $item) {
                    $items[] = $item;
                }
                return array($items);
            }));

        $t->expects($this->exactly(2))
            ->method('transfer');

        $flush = new FlushingBatch($batch, 3);
        $this->assertEquals(3, $flush->getThreshold());
        $flush->setThreshold(2);
        $flush->add('foo')->add('baz')->add('bar')->add('bee')->add('boo');
        $this->assertEquals(1, count($flush));
    }
}
