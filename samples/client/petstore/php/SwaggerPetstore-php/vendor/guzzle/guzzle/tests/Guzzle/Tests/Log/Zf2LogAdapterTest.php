<?php

namespace Guzzle\Tests\Log;

use Guzzle\Log\Zf2LogAdapter;
use Zend\Log\Logger;
use Zend\Log\Writer\Stream;

/**
 * @covers Guzzle\Log\Zf2LogAdapter
 */
class Zf2LogAdapterTest extends \Guzzle\Tests\GuzzleTestCase
{
    /** @var Zf2LogAdapter */
    protected $adapter;

    /** @var Logger */
    protected $log;

    /** @var resource */
    protected $stream;

    protected function setUp()
    {
        $this->stream = fopen('php://temp', 'r+');
        $this->log = new Logger();
        $this->log->addWriter(new Stream($this->stream));
        $this->adapter = new Zf2LogAdapter($this->log);

    }

    public function testLogsMessagesToAdaptedObject()
    {
        // Test without a priority
        $this->adapter->log('Zend_Test!', \LOG_NOTICE);
        rewind($this->stream);
        $contents = stream_get_contents($this->stream);
        $this->assertEquals(1, substr_count($contents, 'Zend_Test!'));

        // Test with a priority
        $this->adapter->log('Zend_Test!', \LOG_ALERT);
        rewind($this->stream);
        $contents = stream_get_contents($this->stream);
        $this->assertEquals(2, substr_count($contents, 'Zend_Test!'));
    }

    public function testExposesAdaptedLogObject()
    {
        $this->assertEquals($this->log, $this->adapter->getLogObject());
    }
}
