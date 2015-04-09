<?php

/*
 * This file is part of the Symfony package.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Symfony\Component\Yaml\Tests;

use Symfony\Component\Yaml\Inline;

class InlineTest extends \PHPUnit_Framework_TestCase
{
    /**
     * @dataProvider getTestsForParse
     */
    public function testParse($yaml, $value)
    {
        $this->assertSame($value, Inline::parse($yaml), sprintf('::parse() converts an inline YAML to a PHP structure (%s)', $yaml));
    }

    /**
     * @dataProvider getTestsForParseWithMapObjects
     */
    public function testParseWithMapObjects($yaml, $value)
    {
        $actual = Inline::parse($yaml, false, false, true);

        $this->assertSame(serialize($value), serialize($actual));
    }

    /**
     * @dataProvider getTestsForDump
     */
    public function testDump($yaml, $value)
    {
        $this->assertEquals($yaml, Inline::dump($value), sprintf('::dump() converts a PHP structure to an inline YAML (%s)', $yaml));

        $this->assertSame($value, Inline::parse(Inline::dump($value)), 'check consistency');
    }

    public function testDumpNumericValueWithLocale()
    {
        $locale = setlocale(LC_NUMERIC, 0);
        if (false === $locale) {
            $this->markTestSkipped('Your platform does not support locales.');
        }

        $required_locales = array('fr_FR.UTF-8', 'fr_FR.UTF8', 'fr_FR.utf-8', 'fr_FR.utf8', 'French_France.1252');
        if (false === setlocale(LC_ALL, $required_locales)) {
            $this->markTestSkipped('Could not set any of required locales: '.implode(', ', $required_locales));
        }

        $this->assertEquals('1.2', Inline::dump(1.2));
        $this->assertContains('fr', strtolower(setlocale(LC_NUMERIC, 0)));

        setlocale(LC_ALL, $locale);
    }

    public function testHashStringsResemblingExponentialNumericsShouldNotBeChangedToINF()
    {
        $value = '686e444';

        $this->assertSame($value, Inline::parse(Inline::dump($value)));
    }

    /**
     * @expectedException \Symfony\Component\Yaml\Exception\ParseException
     */
    public function testParseScalarWithIncorrectlyQuotedStringShouldThrowException()
    {
        $value = "'don't do somthin' like that'";
        Inline::parse($value);
    }

    /**
     * @expectedException \Symfony\Component\Yaml\Exception\ParseException
     */
    public function testParseScalarWithIncorrectlyDoubleQuotedStringShouldThrowException()
    {
        $value = '"don"t do somthin" like that"';
        Inline::parse($value);
    }

    /**
     * @expectedException \Symfony\Component\Yaml\Exception\ParseException
     */
    public function testParseInvalidMappingKeyShouldThrowException()
    {
        $value = '{ "foo " bar": "bar" }';
        Inline::parse($value);
    }

    /**
     * @expectedException \Symfony\Component\Yaml\Exception\ParseException
     */
    public function testParseInvalidMappingShouldThrowException()
    {
        Inline::parse('[foo] bar');
    }

    /**
     * @expectedException \Symfony\Component\Yaml\Exception\ParseException
     */
    public function testParseInvalidSequenceShouldThrowException()
    {
        Inline::parse('{ foo: bar } bar');
    }

    public function testParseScalarWithCorrectlyQuotedStringShouldReturnString()
    {
        $value = "'don''t do somthin'' like that'";
        $expect = "don't do somthin' like that";

        $this->assertSame($expect, Inline::parseScalar($value));
    }

    /**
     * @dataProvider getDataForParseReferences
     */
    public function testParseReferences($yaml, $expected)
    {
        $this->assertSame($expected, Inline::parse($yaml, false, false, false, array('var' => 'var-value')));
    }

    public function getDataForParseReferences()
    {
        return array(
            'scalar' => array('*var', 'var-value'),
            'list' => array('[ *var ]', array('var-value')),
            'list-in-list' => array('[[ *var ]]', array(array('var-value'))),
            'map-in-list' => array('[ { key: *var } ]', array(array('key' => 'var-value'))),
            'embedded-mapping-in-list' => array('[ key: *var ]', array(array('key' => 'var-value'))),
            'map' => array('{ key: *var }', array('key' => 'var-value')),
            'list-in-map' => array('{ key: [*var] }', array('key' => array('var-value'))),
            'map-in-map' => array('{ foo: { bar: *var } }', array('foo' => array('bar' => 'var-value'))),
        );
    }

    public function testParseMapReferenceInSequence()
    {
        $foo = array(
            'a' => 'Steve',
            'b' => 'Clark',
            'c' => 'Brian',
        );
        $this->assertSame(array($foo), Inline::parse('[*foo]', false, false, false, array('foo' => $foo)));
    }

    /**
     * @expectedException \Symfony\Component\Yaml\Exception\ParseException
     * @expectedExceptionMessage A reference must contain at least one character.
     */
    public function testParseUnquotedAsterisk()
    {
        Inline::parse('{ foo: * }');
    }

    /**
     * @expectedException \Symfony\Component\Yaml\Exception\ParseException
     * @expectedExceptionMessage A reference must contain at least one character.
     */
    public function testParseUnquotedAsteriskFollowedByAComment()
    {
        Inline::parse('{ foo: * #foo }');
    }

    public function getTestsForParse()
    {
        return array(
            array('', ''),
            array('null', null),
            array('false', false),
            array('true', true),
            array('12', 12),
            array('-12', -12),
            array('"quoted string"', 'quoted string'),
            array("'quoted string'", 'quoted string'),
            array('12.30e+02', 12.30e+02),
            array('0x4D2', 0x4D2),
            array('02333', 02333),
            array('.Inf', -log(0)),
            array('-.Inf', log(0)),
            array("'686e444'", '686e444'),
            array('686e444', 646e444),
            array('123456789123456789123456789123456789', '123456789123456789123456789123456789'),
            array('"foo\r\nbar"', "foo\r\nbar"),
            array("'foo#bar'", 'foo#bar'),
            array("'foo # bar'", 'foo # bar'),
            array("'#cfcfcf'", '#cfcfcf'),
            array('::form_base.html.twig', '::form_base.html.twig'),

            // Pre-YAML-1.2 booleans
            array("'y'", 'y'),
            array("'n'", 'n'),
            array("'yes'", 'yes'),
            array("'no'", 'no'),
            array("'on'", 'on'),
            array("'off'", 'off'),

            array('2007-10-30', mktime(0, 0, 0, 10, 30, 2007)),
            array('2007-10-30T02:59:43Z', gmmktime(2, 59, 43, 10, 30, 2007)),
            array('2007-10-30 02:59:43 Z', gmmktime(2, 59, 43, 10, 30, 2007)),
            array('1960-10-30 02:59:43 Z', gmmktime(2, 59, 43, 10, 30, 1960)),
            array('1730-10-30T02:59:43Z', gmmktime(2, 59, 43, 10, 30, 1730)),

            array('"a \\"string\\" with \'quoted strings inside\'"', 'a "string" with \'quoted strings inside\''),
            array("'a \"string\" with ''quoted strings inside'''", 'a "string" with \'quoted strings inside\''),

            // sequences
            // urls are no key value mapping. see #3609. Valid yaml "key: value" mappings require a space after the colon
            array('[foo, http://urls.are/no/mappings, false, null, 12]', array('foo', 'http://urls.are/no/mappings', false, null, 12)),
            array('[  foo  ,   bar , false  ,  null     ,  12  ]', array('foo', 'bar', false, null, 12)),
            array('[\'foo,bar\', \'foo bar\']', array('foo,bar', 'foo bar')),

            // mappings
            array('{foo:bar,bar:foo,false:false,null:null,integer:12}', array('foo' => 'bar', 'bar' => 'foo', 'false' => false, 'null' => null, 'integer' => 12)),
            array('{ foo  : bar, bar : foo,  false  :   false,  null  :   null,  integer :  12  }', array('foo' => 'bar', 'bar' => 'foo', 'false' => false, 'null' => null, 'integer' => 12)),
            array('{foo: \'bar\', bar: \'foo: bar\'}', array('foo' => 'bar', 'bar' => 'foo: bar')),
            array('{\'foo\': \'bar\', "bar": \'foo: bar\'}', array('foo' => 'bar', 'bar' => 'foo: bar')),
            array('{\'foo\'\'\': \'bar\', "bar\"": \'foo: bar\'}', array('foo\'' => 'bar', "bar\"" => 'foo: bar')),
            array('{\'foo: \': \'bar\', "bar: ": \'foo: bar\'}', array('foo: ' => 'bar', "bar: " => 'foo: bar')),

            // nested sequences and mappings
            array('[foo, [bar, foo]]', array('foo', array('bar', 'foo'))),
            array('[foo, {bar: foo}]', array('foo', array('bar' => 'foo'))),
            array('{ foo: {bar: foo} }', array('foo' => array('bar' => 'foo'))),
            array('{ foo: [bar, foo] }', array('foo' => array('bar', 'foo'))),

            array('[  foo, [  bar, foo  ]  ]', array('foo', array('bar', 'foo'))),

            array('[{ foo: {bar: foo} }]', array(array('foo' => array('bar' => 'foo')))),

            array('[foo, [bar, [foo, [bar, foo]], foo]]', array('foo', array('bar', array('foo', array('bar', 'foo')), 'foo'))),

            array('[foo, {bar: foo, foo: [foo, {bar: foo}]}, [foo, {bar: foo}]]', array('foo', array('bar' => 'foo', 'foo' => array('foo', array('bar' => 'foo'))), array('foo', array('bar' => 'foo')))),

            array('[foo, bar: { foo: bar }]', array('foo', '1' => array('bar' => array('foo' => 'bar')))),
            array('[foo, \'@foo.baz\', { \'%foo%\': \'foo is %foo%\', bar: \'%foo%\' }, true, \'@service_container\']', array('foo', '@foo.baz', array('%foo%' => 'foo is %foo%', 'bar' => '%foo%'), true, '@service_container')),
        );
    }

    public function getTestsForParseWithMapObjects()
    {
        return array(
            array('', ''),
            array('null', null),
            array('false', false),
            array('true', true),
            array('12', 12),
            array('-12', -12),
            array('"quoted string"', 'quoted string'),
            array("'quoted string'", 'quoted string'),
            array('12.30e+02', 12.30e+02),
            array('0x4D2', 0x4D2),
            array('02333', 02333),
            array('.Inf', -log(0)),
            array('-.Inf', log(0)),
            array("'686e444'", '686e444'),
            array('686e444', 646e444),
            array('123456789123456789123456789123456789', '123456789123456789123456789123456789'),
            array('"foo\r\nbar"', "foo\r\nbar"),
            array("'foo#bar'", 'foo#bar'),
            array("'foo # bar'", 'foo # bar'),
            array("'#cfcfcf'", '#cfcfcf'),
            array('::form_base.html.twig', '::form_base.html.twig'),

            array('2007-10-30', mktime(0, 0, 0, 10, 30, 2007)),
            array('2007-10-30T02:59:43Z', gmmktime(2, 59, 43, 10, 30, 2007)),
            array('2007-10-30 02:59:43 Z', gmmktime(2, 59, 43, 10, 30, 2007)),
            array('1960-10-30 02:59:43 Z', gmmktime(2, 59, 43, 10, 30, 1960)),
            array('1730-10-30T02:59:43Z', gmmktime(2, 59, 43, 10, 30, 1730)),

            array('"a \\"string\\" with \'quoted strings inside\'"', 'a "string" with \'quoted strings inside\''),
            array("'a \"string\" with ''quoted strings inside'''", 'a "string" with \'quoted strings inside\''),

            // sequences
            // urls are no key value mapping. see #3609. Valid yaml "key: value" mappings require a space after the colon
            array('[foo, http://urls.are/no/mappings, false, null, 12]', array('foo', 'http://urls.are/no/mappings', false, null, 12)),
            array('[  foo  ,   bar , false  ,  null     ,  12  ]', array('foo', 'bar', false, null, 12)),
            array('[\'foo,bar\', \'foo bar\']', array('foo,bar', 'foo bar')),

            // mappings
            array('{foo:bar,bar:foo,false:false,null:null,integer:12}', (object) array('foo' => 'bar', 'bar' => 'foo', 'false' => false, 'null' => null, 'integer' => 12)),
            array('{ foo  : bar, bar : foo,  false  :   false,  null  :   null,  integer :  12  }', (object) array('foo' => 'bar', 'bar' => 'foo', 'false' => false, 'null' => null, 'integer' => 12)),
            array('{foo: \'bar\', bar: \'foo: bar\'}', (object) array('foo' => 'bar', 'bar' => 'foo: bar')),
            array('{\'foo\': \'bar\', "bar": \'foo: bar\'}', (object) array('foo' => 'bar', 'bar' => 'foo: bar')),
            array('{\'foo\'\'\': \'bar\', "bar\"": \'foo: bar\'}', (object) array('foo\'' => 'bar', "bar\"" => 'foo: bar')),
            array('{\'foo: \': \'bar\', "bar: ": \'foo: bar\'}', (object) array('foo: ' => 'bar', "bar: " => 'foo: bar')),

            // nested sequences and mappings
            array('[foo, [bar, foo]]', array('foo', array('bar', 'foo'))),
            array('[foo, {bar: foo}]', array('foo', (object) array('bar' => 'foo'))),
            array('{ foo: {bar: foo} }', (object) array('foo' => (object) array('bar' => 'foo'))),
            array('{ foo: [bar, foo] }', (object) array('foo' => array('bar', 'foo'))),

            array('[  foo, [  bar, foo  ]  ]', array('foo', array('bar', 'foo'))),

            array('[{ foo: {bar: foo} }]', array((object) array('foo' => (object) array('bar' => 'foo')))),

            array('[foo, [bar, [foo, [bar, foo]], foo]]', array('foo', array('bar', array('foo', array('bar', 'foo')), 'foo'))),

            array('[foo, {bar: foo, foo: [foo, {bar: foo}]}, [foo, {bar: foo}]]', array('foo', (object) array('bar' => 'foo', 'foo' => array('foo', (object) array('bar' => 'foo'))), array('foo', (object) array('bar' => 'foo')))),

            array('[foo, bar: { foo: bar }]', array('foo', '1' => (object) array('bar' => (object) array('foo' => 'bar')))),
            array('[foo, \'@foo.baz\', { \'%foo%\': \'foo is %foo%\', bar: \'%foo%\' }, true, \'@service_container\']', array('foo', '@foo.baz', (object) array('%foo%' => 'foo is %foo%', 'bar' => '%foo%'), true, '@service_container')),

            array('{}', new \stdClass()),
            array('{ foo  : bar, bar : {}  }', (object) array('foo' => 'bar', 'bar' => new \stdClass())),
            array('{ foo  : [], bar : {}  }', (object) array('foo' => array(), 'bar' => new \stdClass())),
            array('{foo: \'bar\', bar: {} }', (object) array('foo' => 'bar', 'bar' => new \stdClass())),
            array('{\'foo\': \'bar\', "bar": {}}', (object) array('foo' => 'bar', 'bar' => new \stdClass())),
            array('{\'foo\': \'bar\', "bar": \'{}\'}', (object) array('foo' => 'bar', 'bar' => '{}')),

            array('[foo, [{}, {}]]', array('foo', array(new \stdClass(), new \stdClass()))),
            array('[foo, [[], {}]]', array('foo', array(array(), new \stdClass()))),
            array('[foo, [[{}, {}], {}]]', array('foo', array(array(new \stdClass(), new \stdClass()), new \stdClass()))),
            array('[foo, {bar: {}}]', array('foo', '1' => (object) array('bar' => new \stdClass()))),
        );
    }

    public function getTestsForDump()
    {
        return array(
            array('null', null),
            array('false', false),
            array('true', true),
            array('12', 12),
            array("'quoted string'", 'quoted string'),
            array('!!float 1230', 12.30e+02),
            array('1234', 0x4D2),
            array('1243', 02333),
            array('.Inf', -log(0)),
            array('-.Inf', log(0)),
            array("'686e444'", '686e444'),
            array('"foo\r\nbar"', "foo\r\nbar"),
            array("'foo#bar'", 'foo#bar'),
            array("'foo # bar'", 'foo # bar'),
            array("'#cfcfcf'", '#cfcfcf'),

            array("'a \"string\" with ''quoted strings inside'''", 'a "string" with \'quoted strings inside\''),

            array("'-dash'", '-dash'),
            array("'-'", '-'),

            // Pre-YAML-1.2 booleans
            array("'y'", 'y'),
            array("'n'", 'n'),
            array("'yes'", 'yes'),
            array("'no'", 'no'),
            array("'on'", 'on'),
            array("'off'", 'off'),

            // sequences
            array('[foo, bar, false, null, 12]', array('foo', 'bar', false, null, 12)),
            array('[\'foo,bar\', \'foo bar\']', array('foo,bar', 'foo bar')),

            // mappings
            array('{ foo: bar, bar: foo, \'false\': false, \'null\': null, integer: 12 }', array('foo' => 'bar', 'bar' => 'foo', 'false' => false, 'null' => null, 'integer' => 12)),
            array('{ foo: bar, bar: \'foo: bar\' }', array('foo' => 'bar', 'bar' => 'foo: bar')),

            // nested sequences and mappings
            array('[foo, [bar, foo]]', array('foo', array('bar', 'foo'))),

            array('[foo, [bar, [foo, [bar, foo]], foo]]', array('foo', array('bar', array('foo', array('bar', 'foo')), 'foo'))),

            array('{ foo: { bar: foo } }', array('foo' => array('bar' => 'foo'))),

            array('[foo, { bar: foo }]', array('foo', array('bar' => 'foo'))),

            array('[foo, { bar: foo, foo: [foo, { bar: foo }] }, [foo, { bar: foo }]]', array('foo', array('bar' => 'foo', 'foo' => array('foo', array('bar' => 'foo'))), array('foo', array('bar' => 'foo')))),

            array('[foo, \'@foo.baz\', { \'%foo%\': \'foo is %foo%\', bar: \'%foo%\' }, true, \'@service_container\']', array('foo', '@foo.baz', array('%foo%' => 'foo is %foo%', 'bar' => '%foo%'), true, '@service_container')),
        );
    }
}
