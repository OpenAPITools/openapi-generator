<?php

namespace Swagger\Client;

class HeaderSelectorTest extends \PHPUnit_Framework_TestCase
{
    public function testSelectingHeaders()
    {
        $selector = new HeaderSelector();
        $headers = $selector->selectHeaders([
            'application/xml',
            'application/json'
        ], []);
        $this->assertSame('application/json', $headers['Accept']);

        $headers = $selector->selectHeaders([], []);
        $this->assertArrayNotHasKey('Accept', $headers);

        $header = $selector->selectHeaders([
            'application/yaml',
            'application/xml'
        ], []);
        $this->assertSame('application/yaml,application/xml', $header['Accept']);

        // test selectHeaderContentType
        $headers = $selector->selectHeaders([], [
            'application/xml',
            'application/json'
        ]);
        $this->assertSame('application/json', $headers['Content-Type']);

        $headers = $selector->selectHeaders([], []);
        $this->assertSame('application/json', $headers['Content-Type']);
        $headers = $selector->selectHeaders([], [
            'application/yaml',
            'application/xml'
        ]);
        $this->assertSame('application/yaml,application/xml', $headers['Content-Type']);
    }

    public function testSelectingHeadersForMultipartBody()
    {
        // test selectHeaderAccept
        $selector = new HeaderSelector();
        $headers = $selector->selectHeadersForMultipart([
            'application/xml',
            'application/json'
        ]);
        $this->assertSame('application/json', $headers['Accept']);
        $this->assertArrayNotHasKey('Content-Type', $headers);

        $headers = $selector->selectHeadersForMultipart([]);
        $this->assertArrayNotHasKey('Accept', $headers);
        $this->assertArrayNotHasKey('Content-Type', $headers);
    }
}
