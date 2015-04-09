<?php

namespace Guzzle\Tests\Batch;

use Guzzle\Batch\BatchBuilder;

/**
 * @covers Guzzle\Batch\BatchBuilder
 */
class BatchBuilderTest extends \Guzzle\Tests\GuzzleTestCase
{
    private function getMockTransfer()
    {
        return $this->getMock('Guzzle\Batch\BatchTransferInterface');
    }

    private function getMockDivisor()
    {
        return $this->getMock('Guzzle\Batch\BatchDivisorInterface');
    }

    private function getMockBatchBuilder()
    {
        return BatchBuilder::factory()
            ->transferWith($this->getMockTransfer())
            ->createBatchesWith($this->getMockDivisor());
    }

    public function testFactoryCreatesInstance()
    {
        $builder = BatchBuilder::factory();
        $this->assertInstanceOf('Guzzle\Batch\BatchBuilder', $builder);
    }

    public function testAddsAutoFlush()
    {
        $batch = $this->getMockBatchBuilder()->autoFlushAt(10)->build();
        $this->assertInstanceOf('Guzzle\Batch\FlushingBatch', $batch);
    }

    public function testAddsExceptionBuffering()
    {
        $batch = $this->getMockBatchBuilder()->bufferExceptions()->build();
        $this->assertInstanceOf('Guzzle\Batch\ExceptionBufferingBatch', $batch);
    }

    public function testAddHistory()
    {
        $batch = $this->getMockBatchBuilder()->keepHistory()->build();
        $this->assertInstanceOf('Guzzle\Batch\HistoryBatch', $batch);
    }

    public function testAddsNotify()
    {
        $batch = $this->getMockBatchBuilder()->notify(function() {})->build();
        $this->assertInstanceOf('Guzzle\Batch\NotifyingBatch', $batch);
    }

    /**
     * @expectedException Guzzle\Common\Exception\RuntimeException
     */
    public function testTransferStrategyMustBeSet()
    {
        $batch = BatchBuilder::factory()->createBatchesWith($this->getMockDivisor())->build();
    }

    /**
     * @expectedException Guzzle\Common\Exception\RuntimeException
     */
    public function testDivisorStrategyMustBeSet()
    {
        $batch = BatchBuilder::factory()->transferWith($this->getMockTransfer())->build();
    }

    public function testTransfersRequests()
    {
        $batch = BatchBuilder::factory()->transferRequests(10)->build();
        $this->assertInstanceOf('Guzzle\Batch\BatchRequestTransfer', $this->readAttribute($batch, 'transferStrategy'));
    }

    public function testTransfersCommands()
    {
        $batch = BatchBuilder::factory()->transferCommands(10)->build();
        $this->assertInstanceOf('Guzzle\Batch\BatchCommandTransfer', $this->readAttribute($batch, 'transferStrategy'));
    }
}
