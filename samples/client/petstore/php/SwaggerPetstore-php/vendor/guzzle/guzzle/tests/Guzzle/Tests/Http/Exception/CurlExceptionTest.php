<?php

namespace Guzzle\Tests\Http\Exception;

use Guzzle\Http\Exception\CurlException;
use Guzzle\Http\Curl\CurlHandle;

/**
 * @covers Guzzle\Http\Exception\CurlException
 */
class CurlExceptionTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testStoresCurlError()
    {
        $e = new CurlException();
        $this->assertNull($e->getError());
        $this->assertNull($e->getErrorNo());
        $this->assertSame($e, $e->setError('test', 12));
        $this->assertEquals('test', $e->getError());
        $this->assertEquals(12, $e->getErrorNo());

        $handle = new CurlHandle(curl_init(), array());
        $e->setCurlHandle($handle);
        $this->assertSame($handle, $e->getCurlHandle());
        $handle->close();
    }
}
