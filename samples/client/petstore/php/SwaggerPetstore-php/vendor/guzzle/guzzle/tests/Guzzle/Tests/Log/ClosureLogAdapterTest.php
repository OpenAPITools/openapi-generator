<?php

namespace Guzzle\Tests\Log;

use Guzzle\Log\ClosureLogAdapter;

/**
 * @covers Guzzle\Log\ClosureLogAdapter
 */
class ClosureLogAdapterTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testClosure()
    {
        $that = $this;
        $modified = null;
        $this->adapter = new ClosureLogAdapter(function($message, $priority, $extras = null) use ($that, &$modified) {
            $modified = array($message, $priority, $extras);
        });
        $this->adapter->log('test', LOG_NOTICE, '127.0.0.1');
        $this->assertEquals(array('test', LOG_NOTICE, '127.0.0.1'), $modified);
    }

    /**
     * @expectedException InvalidArgumentException
     */
    public function testThrowsExceptionWhenNotCallable()
    {
        $this->adapter = new ClosureLogAdapter(123);
    }
}
