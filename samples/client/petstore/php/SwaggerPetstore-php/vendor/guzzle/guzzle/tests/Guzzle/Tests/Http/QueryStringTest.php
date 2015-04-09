<?php

namespace Guzzle\Tests\Http;

use Guzzle\Http\QueryString;
use Guzzle\Http\QueryAggregator\DuplicateAggregator;
use Guzzle\Http\QueryAggregator\CommaAggregator;

class QueryStringTest extends \Guzzle\Tests\GuzzleTestCase
{
    /** @var \Guzzle\Http\QueryString The query string object to test */
    protected $q;

    public function setup()
    {
        $this->q = new QueryString();
    }

    public function testGetFieldSeparator()
    {
        $this->assertEquals('&', $this->q->getFieldSeparator());
    }

    public function testGetValueSeparator()
    {
        $this->assertEquals('=', $this->q->getValueSeparator());
    }

    public function testIsUrlEncoding()
    {
        $this->assertEquals('RFC 3986', $this->q->getUrlEncoding());
        $this->assertTrue($this->q->isUrlEncoding());
        $this->assertEquals('foo%20bar', $this->q->encodeValue('foo bar'));

        $this->q->useUrlEncoding(QueryString::FORM_URLENCODED);
        $this->assertTrue($this->q->isUrlEncoding());
        $this->assertEquals(QueryString::FORM_URLENCODED, $this->q->getUrlEncoding());
        $this->assertEquals('foo+bar', $this->q->encodeValue('foo bar'));

        $this->assertSame($this->q, $this->q->useUrlEncoding(false));
        $this->assertFalse($this->q->isUrlEncoding());
        $this->assertFalse($this->q->isUrlEncoding());
    }

    public function testSetFieldSeparator()
    {
        $this->assertEquals($this->q, $this->q->setFieldSeparator('/'));
        $this->assertEquals('/', $this->q->getFieldSeparator());
    }

    public function testSetValueSeparator()
    {
        $this->assertEquals($this->q, $this->q->setValueSeparator('/'));
        $this->assertEquals('/', $this->q->getValueSeparator());
    }

    public function testUrlEncode()
    {
        $params = array(
            'test'   => 'value',
            'test 2' => 'this is a test?',
            'test3'  => array('v1', 'v2', 'v3'),
            'ሴ'      => 'bar'
        );
        $encoded = array(
            'test'         => 'value',
            'test%202'     => rawurlencode('this is a test?'),
            'test3%5B0%5D' => 'v1',
            'test3%5B1%5D' => 'v2',
            'test3%5B2%5D' => 'v3',
            '%E1%88%B4'    => 'bar'
        );
        $this->q->replace($params);
        $this->assertEquals($encoded, $this->q->urlEncode());

        // Disable encoding
        $testData = array('test 2' => 'this is a test');
        $this->q->replace($testData);
        $this->q->useUrlEncoding(false);
        $this->assertEquals($testData, $this->q->urlEncode());
    }

    public function testToString()
    {
        // Check with no parameters
        $this->assertEquals('', $this->q->__toString());

        $params = array(
            'test'   => 'value',
            'test 2' => 'this is a test?',
            'test3'  => array('v1', 'v2', 'v3'),
            'test4'  => null,
        );
        $this->q->replace($params);
        $this->assertEquals('test=value&test%202=this%20is%20a%20test%3F&test3%5B0%5D=v1&test3%5B1%5D=v2&test3%5B2%5D=v3&test4', $this->q->__toString());
        $this->q->useUrlEncoding(false);
        $this->assertEquals('test=value&test 2=this is a test?&test3[0]=v1&test3[1]=v2&test3[2]=v3&test4', $this->q->__toString());

        // Use an alternative aggregator
        $this->q->setAggregator(new CommaAggregator());
        $this->assertEquals('test=value&test 2=this is a test?&test3=v1,v2,v3&test4', $this->q->__toString());
    }

    public function testAllowsMultipleValuesPerKey()
    {
        $q = new QueryString();
        $q->add('facet', 'size');
        $q->add('facet', 'width');
        $q->add('facet.field', 'foo');
        // Use the duplicate aggregator
        $q->setAggregator(new DuplicateAggregator());
        $this->assertEquals('facet=size&facet=width&facet.field=foo', $q->__toString());
    }

    public function testAllowsNestedQueryData()
    {
        $this->q->replace(array(
            'test' => 'value',
            't' => array(
                'v1' => 'a',
                'v2' => 'b',
                'v3' => array(
                    'v4' => 'c',
                    'v5' => 'd',
                )
            )
        ));

        $this->q->useUrlEncoding(false);
        $this->assertEquals('test=value&t[v1]=a&t[v2]=b&t[v3][v4]=c&t[v3][v5]=d', $this->q->__toString());
    }

    public function parseQueryProvider()
    {
        return array(
            // Ensure that multiple query string values are allowed per value
            array('q=a&q=b', array('q' => array('a', 'b'))),
            // Ensure that PHP array style query string values are parsed
            array('q[]=a&q[]=b', array('q' => array('a', 'b'))),
            // Ensure that a single PHP array style query string value is parsed into an array
            array('q[]=a', array('q' => array('a'))),
            // Ensure that decimals are allowed in query strings
            array('q.a=a&q.b=b', array(
                'q.a' => 'a',
                'q.b' => 'b'
            )),
            // Ensure that query string values are percent decoded
            array('q%20a=a%20b', array('q a' => 'a b')),
            // Ensure null values can be added
            array('q&a', array('q' => false, 'a' => false)),
        );
    }

    /**
     * @dataProvider parseQueryProvider
     */
    public function testParsesQueryStrings($query, $data)
    {
        $query = QueryString::fromString($query);
        $this->assertEquals($data, $query->getAll());
    }

    public function testProperlyDealsWithDuplicateQueryStringValues()
    {
        $query = QueryString::fromString('foo=a&foo=b&?µ=c');
        $this->assertEquals(array('a', 'b'), $query->get('foo'));
        $this->assertEquals('c', $query->get('?µ'));
    }

    public function testAllowsBlankQueryStringValues()
    {
        $query = QueryString::fromString('foo');
        $this->assertEquals('foo', (string) $query);
        $query->set('foo', QueryString::BLANK);
        $this->assertEquals('foo', (string) $query);
    }

    public function testAllowsFalsyQueryStringValues()
    {
        $query = QueryString::fromString('0');
        $this->assertEquals('0', (string) $query);
        $query->set('0', QueryString::BLANK);
        $this->assertSame('0', (string) $query);
    }

    public function testFromStringIgnoresQuestionMark()
    {
        $query = QueryString::fromString('foo=baz&bar=boo');
        $this->assertEquals('foo=baz&bar=boo', (string) $query);
    }

    public function testConvertsPlusSymbolsToSpaces()
    {
        $query = QueryString::fromString('var=foo+bar');
        $this->assertEquals('foo bar', $query->get('var'));
    }

    public function testFromStringDoesntMangleZeroes()
    {
        $query = QueryString::fromString('var=0');
        $this->assertSame('0', $query->get('var'));
    }

    public function testAllowsZeroValues()
    {
        $query = new QueryString(array(
            'foo' => 0,
            'baz' => '0',
            'bar' => null,
            'boo' => false,
            'bam' => ''
        ));
        $this->assertEquals('foo=0&baz=0&bar&boo&bam=', (string) $query);
    }

    public function testFromStringDoesntStripTrailingEquals()
    {
        $query = QueryString::fromString('data=mF0b3IiLCJUZWFtIERldiJdfX0=');
        $this->assertEquals('mF0b3IiLCJUZWFtIERldiJdfX0=', $query->get('data'));
    }

    public function testGuessesIfDuplicateAggregatorShouldBeUsed()
    {
        $query = QueryString::fromString('test=a&test=b');
        $this->assertEquals('test=a&test=b', (string) $query);
    }

    public function testGuessesIfDuplicateAggregatorShouldBeUsedAndChecksForPhpStyle()
    {
        $query = QueryString::fromString('test[]=a&test[]=b');
        $this->assertEquals('test%5B0%5D=a&test%5B1%5D=b', (string) $query);
    }
}
