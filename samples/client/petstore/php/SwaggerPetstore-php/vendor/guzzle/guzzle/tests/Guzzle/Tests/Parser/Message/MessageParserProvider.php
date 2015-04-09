<?php

namespace Guzzle\Tests\Parser\Message;

class MessageParserProvider extends \Guzzle\Tests\GuzzleTestCase
{
    public function requestProvider()
    {
        $auth = base64_encode('michael:foo');

        return array(

            // Empty request
            array('', false),

            // Converts casing of request. Does not require host header.
            array("GET / HTTP/1.1\r\n\r\n", array(
                'method'   => 'GET',
                'protocol' => 'HTTP',
                'version' => '1.1',
                'request_url' => array(
                    'scheme' => 'http',
                    'host'   => '',
                    'port'   => '',
                    'path'   => '/',
                    'query'  => ''
                ),
                'headers' => array(),
                'body'    => ''
            )),
            // Path and query string, multiple header values per header and case sensitive storage
            array("HEAD /path?query=foo HTTP/1.0\r\nHost: example.com\r\nX-Foo: foo\r\nx-foo: Bar\r\nX-Foo: foo\r\nX-Foo: Baz\r\n\r\n", array(
                'method'   => 'HEAD',
                'protocol' => 'HTTP',
                'version' => '1.0',
                'request_url' => array(
                    'scheme' => 'http',
                    'host'   => 'example.com',
                    'port'   => '',
                    'path'   => '/path',
                    'query'  => 'query=foo'
                ),
                'headers' => array(
                    'Host'  => 'example.com',
                    'X-Foo' => array('foo', 'foo', 'Baz'),
                    'x-foo' => 'Bar'
                ),
                'body'    => ''
            )),
            // Includes a body
            array("PUT / HTTP/1.0\r\nhost: example.com:443\r\nContent-Length: 4\r\n\r\ntest", array(
                'method'   => 'PUT',
                'protocol' => 'HTTP',
                'version' => '1.0',
                'request_url' => array(
                    'scheme' => 'https',
                    'host'   => 'example.com',
                    'port'   => '443',
                    'path'   => '/',
                    'query'  => ''
                ),
                'headers' => array(
                    'host'           => 'example.com:443',
                    'Content-Length' => '4'
                ),
                'body' => 'test'
            )),
            // Includes Authorization headers
            array("GET / HTTP/1.1\r\nHost: example.com:8080\r\nAuthorization: Basic {$auth}\r\n\r\n", array(
                'method'   => 'GET',
                'protocol' => 'HTTP',
                'version' => '1.1',
                'request_url' => array(
                    'scheme' => 'http',
                    'host'   => 'example.com',
                    'port'   => '8080',
                    'path'   => '/',
                    'query'  => ''
                ),
                'headers' => array(
                    'Host'           => 'example.com:8080',
                    'Authorization' => "Basic {$auth}"
                ),
                'body' => ''
            )),
            // Include authorization header
            array("GET / HTTP/1.1\r\nHost: example.com:8080\r\nauthorization: Basic {$auth}\r\n\r\n", array(
                'method'   => 'GET',
                'protocol' => 'HTTP',
                'version' => '1.1',
                'request_url' => array(
                    'scheme' => 'http',
                    'host'   => 'example.com',
                    'port'   => '8080',
                    'path'   => '/',
                    'query'  => ''
                ),
                'headers' => array(
                    'Host'           => 'example.com:8080',
                    'authorization' => "Basic {$auth}"
                ),
                'body' => ''
            )),
        );
    }

    public function responseProvider()
    {
        return array(
            // Empty request
            array('', false),

            array("HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n", array(
                'protocol'      => 'HTTP',
                'version'       => '1.1',
                'code'          => '200',
                'reason_phrase' => 'OK',
                'headers'       => array(
                    'Content-Length' => 0
                ),
                'body'          => ''
            )),
            array("HTTP/1.0 400 Bad Request\r\nContent-Length: 0\r\n\r\n", array(
                'protocol'      => 'HTTP',
                'version'       => '1.0',
                'code'          => '400',
                'reason_phrase' => 'Bad Request',
                'headers'       => array(
                    'Content-Length' => 0
                ),
                'body'          => ''
            )),
            array("HTTP/1.0 100 Continue\r\n\r\n", array(
                'protocol'      => 'HTTP',
                'version'       => '1.0',
                'code'          => '100',
                'reason_phrase' => 'Continue',
                'headers'       => array(),
                'body'          => ''
            )),
            array("HTTP/1.1 204 No Content\r\nX-Foo: foo\r\nx-foo: Bar\r\nX-Foo: foo\r\n\r\n", array(
                'protocol'      => 'HTTP',
                'version'       => '1.1',
                'code'          => '204',
                'reason_phrase' => 'No Content',
                'headers'       => array(
                    'X-Foo' => array('foo', 'foo'),
                    'x-foo' => 'Bar'
                ),
                'body'          => ''
            )),
            array("HTTP/1.1 200 Ok that is great!\r\nContent-Length: 4\r\n\r\nTest", array(
                'protocol'      => 'HTTP',
                'version'       => '1.1',
                'code'          => '200',
                'reason_phrase' => 'Ok that is great!',
                'headers'       => array(
                    'Content-Length' => 4
                ),
                'body'          => 'Test'
            )),
        );
    }

    public function compareRequestResults($result, $expected)
    {
        if (!$result) {
            $this->assertFalse($expected);
            return;
        }

        $this->assertEquals($result['method'], $expected['method']);
        $this->assertEquals($result['protocol'], $expected['protocol']);
        $this->assertEquals($result['version'], $expected['version']);
        $this->assertEquals($result['request_url'], $expected['request_url']);
        $this->assertEquals($result['body'], $expected['body']);
        $this->compareHttpHeaders($result['headers'], $expected['headers']);
    }

    public function compareResponseResults($result, $expected)
    {
        if (!$result) {
            $this->assertFalse($expected);
            return;
        }

        $this->assertEquals($result['protocol'], $expected['protocol']);
        $this->assertEquals($result['version'], $expected['version']);
        $this->assertEquals($result['code'], $expected['code']);
        $this->assertEquals($result['reason_phrase'], $expected['reason_phrase']);
        $this->assertEquals($result['body'], $expected['body']);
        $this->compareHttpHeaders($result['headers'], $expected['headers']);
    }

    protected function normalizeHeaders($headers)
    {
        $normalized = array();
        foreach ($headers as $key => $value) {
            $key = strtolower($key);
            if (!isset($normalized[$key])) {
                $normalized[$key] = $value;
            } elseif (!is_array($normalized[$key])) {
                $normalized[$key] = array($value);
            } else {
                $normalized[$key][] = $value;
            }
        }

        foreach ($normalized as $key => &$value) {
            if (is_array($value)) {
                sort($value);
            }
        }

        return $normalized;
    }

    public function compareHttpHeaders($result, $expected)
    {
        // Aggregate all headers case-insensitively
        $result = $this->normalizeHeaders($result);
        $expected = $this->normalizeHeaders($expected);
        $this->assertEquals($result, $expected);
    }
}
