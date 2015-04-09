<?php
/*
 * This file is part of PHPUnit.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 *
 *
 * @package    PHPUnit
 * @author     Mike Naberezny <mike@maintainable.com>
 * @author     Derek DeVries <derek@maintainable.com>
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.3.0
 * @covers     PHPUnit_Util_XML
 */
class Util_XMLTest extends PHPUnit_Framework_TestCase
{
    public function testAssertValidKeysValidKeys()
    {
        $options   = array('testA' => 1, 'testB' => 2, 'testC' => 3);
        $valid     = array('testA', 'testB', 'testC');
        $expected  = array('testA' => 1, 'testB' => 2, 'testC' => 3);
        $validated = PHPUnit_Util_XML::assertValidKeys($options, $valid);

        $this->assertEquals($expected, $validated);
    }

    public function testAssertValidKeysValidKeysEmpty()
    {
        $options   = array('testA' => 1, 'testB' => 2);
        $valid     = array('testA', 'testB', 'testC');
        $expected  = array('testA' => 1, 'testB' => 2, 'testC' => null);
        $validated = PHPUnit_Util_XML::assertValidKeys($options, $valid);

        $this->assertEquals($expected, $validated);
    }

    public function testAssertValidKeysDefaultValuesA()
    {
        $options   = array('testA' => 1, 'testB' => 2);
        $valid     = array('testA' => 23, 'testB' => 24, 'testC' => 25);
        $expected  = array('testA' => 1, 'testB' => 2, 'testC' => 25);
        $validated = PHPUnit_Util_XML::assertValidKeys($options, $valid);

        $this->assertEquals($expected, $validated);
    }

    public function testAssertValidKeysDefaultValuesB()
    {
        $options   = array();
        $valid     = array('testA' => 23, 'testB' => 24, 'testC' => 25);
        $expected  = array('testA' => 23, 'testB' => 24, 'testC' => 25);
        $validated = PHPUnit_Util_XML::assertValidKeys($options, $valid);

        $this->assertEquals($expected, $validated);
    }

    public function testAssertValidKeysInvalidKey()
    {
        $options = array('testA' => 1, 'testB' => 2, 'testD' => 3);
        $valid   = array('testA', 'testB', 'testC');

        try {
            $validated = PHPUnit_Util_XML::assertValidKeys($options, $valid);
            $this->fail();
        } catch (PHPUnit_Framework_Exception $e) {
            $this->assertEquals('Unknown key(s): testD', $e->getMessage());
        }
    }

    public function testAssertValidKeysInvalidKeys()
    {
        $options = array('testA' => 1, 'testD' => 2, 'testE' => 3);
        $valid   = array('testA', 'testB', 'testC');

        try {
            $validated = PHPUnit_Util_XML::assertValidKeys($options, $valid);
            $this->fail();
        } catch (PHPUnit_Framework_Exception $e) {
            $this->assertEquals('Unknown key(s): testD, testE', $e->getMessage());
        }
    }

    public function testConvertAssertSelect()
    {
        $selector  = 'div#folder.open a[href="http://www.xerox.com"][title="xerox"].selected.big > span + h1';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('tag'   => 'div',
                           'id'    => 'folder',
                           'class' => 'open',
                           'descendant' => array('tag'        => 'a',
                                                 'class'      => 'selected big',
                                                 'attributes' => array('href'  => 'http://www.xerox.com',
                                                                       'title' => 'xerox'),
                                                 'child'      => array('tag' => 'span',
                                                                       'adjacent-sibling' => array('tag' => 'h1'))));
        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertSelectElt()
    {
        $selector  = 'div';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('tag' => 'div');

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertClass()
    {
        $selector  = '.foo';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('class' => 'foo');

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertId()
    {
        $selector  = '#foo';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('id' => 'foo');

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertAttribute()
    {
        $selector  = '[foo="bar"]';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('attributes' => array('foo' => 'bar'));

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertAttributeSpaces()
    {
        $selector  = '[foo="bar baz"] div[value="foo bar"]';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('attributes' => array('foo' => 'bar baz'),
                           'descendant' => array('tag'        => 'div',
                                                 'attributes' => array('value' => 'foo bar')));
        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertAttributeMultipleSpaces()
    {
        $selector = '[foo="bar baz"] div[value="foo bar baz"]';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag      = array('attributes' => array('foo' => 'bar baz'),
                          'descendant' => array('tag' => 'div',
                                                'attributes' => array('value' => 'foo bar baz')));
        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertSelectEltClass()
    {
        $selector  = 'div.foo';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('tag' => 'div', 'class' => 'foo');

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertSelectEltId()
    {
        $selector  = 'div#foo';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('tag' => 'div', 'id' => 'foo');

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertSelectEltAttrEqual()
    {
        $selector  = 'div[foo="bar"]';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('tag' => 'div', 'attributes' => array('foo' => 'bar'));

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertSelectEltMultiAttrEqual()
    {
        $selector  = 'div[foo="bar"][baz="fob"]';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('tag' => 'div', 'attributes' => array('foo' => 'bar', 'baz' => 'fob'));

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertSelectEltAttrHasOne()
    {
        $selector  = 'div[foo~="bar"]';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('tag' => 'div', 'attributes' => array('foo' => 'regexp:/.*\bbar\b.*/'));

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertSelectEltAttrContains()
    {
        $selector  = 'div[foo*="bar"]';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('tag' => 'div', 'attributes' => array('foo' => 'regexp:/.*bar.*/'));

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertSelectEltChild()
    {
        $selector  = 'div > a';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('tag' => 'div', 'child' => array('tag' => 'a'));

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertSelectEltAdjacentSibling()
    {
        $selector  = 'div + a';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('tag' => 'div', 'adjacent-sibling' => array('tag' => 'a'));

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertSelectEltDescendant()
    {
        $selector  = 'div a';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector);
        $tag       = array('tag' => 'div', 'descendant' => array('tag' => 'a'));

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertSelectContent()
    {
        $selector  = '#foo';
        $content   = 'div contents';
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector, $content);
        $tag       = array('id' => 'foo', 'content' => 'div contents');

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertSelectTrue()
    {
        $selector  = '#foo';
        $content   = true;
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector, $content);
        $tag       = array('id' => 'foo');

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertSelectFalse()
    {
        $selector  = '#foo';
        $content   = false;
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector, $content);
        $tag       = array('id' => 'foo');

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertNumber()
    {
        $selector  = '.foo';
        $content   = 3;
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector, $content);
        $tag       = array('class' => 'foo');

        $this->assertEquals($tag, $converted);
    }

    public function testConvertAssertRange()
    {
        $selector  = '#foo';
        $content   = array('greater_than' => 5, 'less_than' => 10);
        $converted = PHPUnit_Util_XML::convertSelectToTag($selector, $content);
        $tag       = array('id' => 'foo');

        $this->assertEquals($tag, $converted);
    }

    /**
     * @dataProvider charProvider
     */
    public function testPrepareString($char)
    {
        $e = null;

        $escapedString = PHPUnit_Util_XML::prepareString($char);
        $xml = "<?xml version='1.0' encoding='UTF-8' ?><tag>$escapedString</tag>";
        $dom = new DomDocument('1.0', 'UTF-8');

        try {
            $dom->loadXML($xml);
        } catch (Exception $e) {
        }

        $this->assertNull($e, sprintf(
            'PHPUnit_Util_XML::prepareString("\x%02x") should not crash DomDocument',
            ord($char)
        ));
    }

    public function charProvider()
    {
        $data = array();

        for ($i = 0; $i < 256; $i++) {
            $data[] = array(chr($i));
        }

        return $data;
    }
}
