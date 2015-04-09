<?php

namespace Guzzle\Tests\Batch;

use Guzzle\Batch\BatchRequestTransfer;
use Guzzle\Http\Client;
use Guzzle\Http\Curl\CurlMulti;

/**
 * @covers Guzzle\Batch\BatchRequestTransfer
 */
class BatchRequestTransferTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testCreatesBatchesBasedOnCurlMultiHandles()
    {
        $client1 = new Client('http://www.example.com');
        $client1->setCurlMulti(new CurlMulti());

        $client2 = new Client('http://www.example.com');
        $client2->setCurlMulti(new CurlMulti());

        $request1 = $client1->get();
        $request2 = $client2->get();
        $request3 = $client1->get();
        $request4 = $client2->get();
        $request5 = $client1->get();

        $queue = new \SplQueue();
        $queue[] = $request1;
        $queue[] = $request2;
        $queue[] = $request3;
        $queue[] = $request4;
        $queue[] = $request5;

        $batch = new BatchRequestTransfer(2);
        $this->assertEquals(array(
            array($request1, $request3),
            array($request3),
            array($request2, $request4)
        ), $batch->createBatches($queue));
    }

    /**
     * @expectedException \Guzzle\Common\Exception\InvalidArgumentException
     */
    public function testEnsuresAllItemsAreRequests()
    {
        $queue = new \SplQueue();
        $queue[] = 'foo';
        $batch = new BatchRequestTransfer(2);
        $batch->createBatches($queue);
    }

    public function testTransfersBatches()
    {
        $client = new Client('http://127.0.0.1:123');
        $request = $client->get();
        // For some reason... PHP unit clones the request, which emits a request.clone event. This causes the
        // 'sorted' property of the event dispatcher to contain an array in the cloned request that is not present in
        // the original.
        $request->dispatch('request.clone');

        $multi = $this->getMock('Guzzle\Http\Curl\CurlMultiInterface');
        $client->setCurlMulti($multi);
        $multi->expects($this->once())
            ->method('add')
            ->with($request);
        $multi->expects($this->once())
            ->method('send');

        $batch = new BatchRequestTransfer(2);
        $batch->transfer(array($request));
    }

    public function testDoesNotTransfersEmptyBatches()
    {
        $batch = new BatchRequestTransfer(2);
        $batch->transfer(array());
    }
}
