<?php

namespace Guzzle\Tests\Message;

use Guzzle\Common\Collection;
use Guzzle\Tests\Http\Message\HeaderComparison;

class HeaderComparisonTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function filterProvider()
    {
        return array(

            // Headers match
            array(array(
                'Content-Length' => 'Foo'
            ), array(
                'Content-Length' => 'Foo'
            ), false),

            // Missing header
            array(array(
                'X-Foo' => 'Bar'
            ), array(), array(
                '- X-Foo' => 'Bar'
            )),

            // Extra headers is present
            array(array(
                'X-Foo' => 'Bar'
            ), array(
                'X-Foo' => 'Bar',
                'X-Baz' => 'Jar'
            ), array(
                '+ X-Baz' => 'Jar'
            )),

            // Header is present but must be absent
            array(array(
                '!X-Foo' => '*'
            ), array(
                'X-Foo' => 'Bar'
            ), array(
                '++ X-Foo' => 'Bar'
            )),

            // Different values
            array(array(
                'X-Foo' => 'Bar'
            ), array(
                'X-Foo' => 'Baz'
            ), array(
                'X-Foo' => 'Baz != Bar'
            )),

            // Wildcard search passes
            array(array(
                'X-Foo' => '*'
            ), array(
                'X-Foo' => 'Bar'
            ), false),

            // Wildcard search fails
            array(array(
                'X-Foo' => '*'
            ), array(), array(
                '- X-Foo' => '*'
            )),

            // Ignore extra header if present
            array(array(
                'X-Foo' => '*',
                '_X-Bar' => '*',
            ), array(
                'X-Foo' => 'Baz',
                'X-Bar' => 'Jar'
            ), false),

            // Ignore extra header if present and is not
            array(array(
                'X-Foo' => '*',
                '_X-Bar' => '*',
            ), array(
                'X-Foo' => 'Baz'
            ), false),

            // Case insensitive
            array(array(
                'X-Foo' => '*',
                '_X-Bar' => '*',
            ), array(
                'x-foo' => 'Baz',
                'x-BAR' => 'baz'
            ), false),

            // Case insensitive with collection
            array(array(
                'X-Foo' => '*',
                '_X-Bar' => '*',
            ), new Collection(array(
                'x-foo' => 'Baz',
                'x-BAR' => 'baz'
            )), false),
        );
    }

    /**
     * @dataProvider filterProvider
     */
    public function testComparesHeaders($filters, $headers, $result)
    {
        $compare = new HeaderComparison();
        $this->assertEquals($result, $compare->compare($filters, $headers));
    }
}
