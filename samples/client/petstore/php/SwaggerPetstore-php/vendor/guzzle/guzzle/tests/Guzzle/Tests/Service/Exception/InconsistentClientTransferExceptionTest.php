<?php

namespace Guzzle\Tests\Service\Exception;

use Guzzle\Service\Exception\InconsistentClientTransferException;

class InconsistentClientTransferExceptionTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testStoresCommands()
    {
        $items = array('foo', 'bar');
        $e = new InconsistentClientTransferException($items);
        $this->assertEquals($items, $e->getCommands());
    }
}
