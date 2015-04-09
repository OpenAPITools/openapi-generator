<?php

namespace Guzzle\Tests\Common\Exception;

use Guzzle\Batch\Exception\BatchTransferException;

class BatchTransferExceptionTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testContainsBatch()
    {
        $e = new \Exception('Baz!');
        $t = $this->getMock('Guzzle\Batch\BatchTransferInterface');
        $d = $this->getMock('Guzzle\Batch\BatchDivisorInterface');
        $transferException = new BatchTransferException(array('foo'), array(1, 2), $e, $t, $d);
        $this->assertEquals(array('foo'), $transferException->getBatch());
        $this->assertSame($t, $transferException->getTransferStrategy());
        $this->assertSame($d, $transferException->getDivisorStrategy());
        $this->assertSame($e, $transferException->getPrevious());
        $this->assertEquals(array(1, 2), $transferException->getTransferredItems());
    }
}
