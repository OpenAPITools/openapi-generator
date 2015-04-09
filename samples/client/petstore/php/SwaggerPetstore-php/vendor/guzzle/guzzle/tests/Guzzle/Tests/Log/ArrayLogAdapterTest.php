<?php

namespace Guzzle\Tests\Log;

use Guzzle\Log\ArrayLogAdapter;

class ArrayLogAdapterTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testLog()
    {
        $adapter = new ArrayLogAdapter();
        $adapter->log('test', \LOG_NOTICE, '127.0.0.1');
        $this->assertEquals(array(array('message' => 'test', 'priority' => \LOG_NOTICE, 'extras' => '127.0.0.1')), $adapter->getLogs());
    }

    public function testClearLog()
    {
        $adapter = new ArrayLogAdapter();
        $adapter->log('test', \LOG_NOTICE, '127.0.0.1');
        $adapter->clearLogs();
        $this->assertEquals(array(), $adapter->getLogs());
    }
}
