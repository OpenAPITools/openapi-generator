<?php

namespace Guzzle\Tests\Batch;

use Guzzle\Batch\NotifyingBatch;
use Guzzle\Batch\Batch;

/**
 * @covers Guzzle\Batch\NotifyingBatch
 */
class NotifyingBatchTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testNotifiesAfterFlush()
    {
        $batch = $this->getMock('Guzzle\Batch\Batch', array('flush'), array(
            $this->getMock('Guzzle\Batch\BatchTransferInterface'),
            $this->getMock('Guzzle\Batch\BatchDivisorInterface')
        ));

        $batch->expects($this->once())
            ->method('flush')
            ->will($this->returnValue(array('foo', 'baz')));

        $data = array();
        $decorator = new NotifyingBatch($batch, function ($batch) use (&$data) {
            $data[] = $batch;
        });

        $decorator->add('foo')->add('baz');
        $decorator->flush();
        $this->assertEquals(array(array('foo', 'baz')), $data);
    }

    /**
     * @expectedException Guzzle\Common\Exception\InvalidArgumentException
     */
    public function testEnsuresCallableIsValid()
    {
        $batch = new Batch(
            $this->getMock('Guzzle\Batch\BatchTransferInterface'),
            $this->getMock('Guzzle\Batch\BatchDivisorInterface')
        );
        $decorator = new NotifyingBatch($batch, 'foo');
    }
}
