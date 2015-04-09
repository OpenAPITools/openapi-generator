<?php

namespace Guzzle\Tests\Service\Description;

use Guzzle\Service\Description\Parameter;
use Guzzle\Service\Description\SchemaValidator;

/**
 * @covers Guzzle\Service\Description\SchemaValidator
 */
class SchemaValidatorTest extends \Guzzle\Tests\GuzzleTestCase
{
    /** @var SchemaValidator */
    protected $validator;

    public function setUp()
    {
        $this->validator = new SchemaValidator();
    }

    public function testValidatesArrayListsAreNumericallyIndexed()
    {
        $value = array(array(1));
        $this->assertFalse($this->validator->validate($this->getComplexParam(), $value));
        $this->assertEquals(
            array('[Foo][0] must be an array of properties. Got a numerically indexed array.'),
            $this->validator->getErrors()
        );
    }

    public function testValidatesArrayListsContainProperItems()
    {
        $value = array(true);
        $this->assertFalse($this->validator->validate($this->getComplexParam(), $value));
        $this->assertEquals(
            array('[Foo][0] must be of type object'),
            $this->validator->getErrors()
        );
    }

    public function testAddsDefaultValuesInLists()
    {
        $value = array(array());
        $this->assertTrue($this->validator->validate($this->getComplexParam(), $value));
        $this->assertEquals(array(array('Bar' => true)), $value);
    }

    public function testMergesDefaultValuesInLists()
    {
        $value = array(
            array('Baz' => 'hello!'),
            array('Bar' => false)
        );
        $this->assertTrue($this->validator->validate($this->getComplexParam(), $value));
        $this->assertEquals(array(
            array(
                'Baz' => 'hello!',
                'Bar' => true
            ),
            array('Bar' => false)
        ), $value);
    }

    public function testCorrectlyConvertsParametersToArrayWhenArraysArePresent()
    {
        $param = $this->getComplexParam();
        $result = $param->toArray();
        $this->assertInternalType('array', $result['items']);
        $this->assertEquals('array', $result['type']);
        $this->assertInstanceOf('Guzzle\Service\Description\Parameter', $param->getItems());
    }

    public function testAllowsInstanceOf()
    {
        $p = new Parameter(array(
            'name'       => 'foo',
            'type'       => 'object',
            'instanceOf' => get_class($this)
        ));
        $this->assertTrue($this->validator->validate($p, $this));
        $this->assertFalse($this->validator->validate($p, $p));
        $this->assertEquals(array('[foo] must be an instance of ' . __CLASS__), $this->validator->getErrors());
    }

    public function testEnforcesInstanceOfOnlyWhenObject()
    {
        $p = new Parameter(array(
            'name'       => 'foo',
            'type'       => array('object', 'string'),
            'instanceOf' => get_class($this)
        ));
        $this->assertTrue($this->validator->validate($p, $this));
        $s = 'test';
        $this->assertTrue($this->validator->validate($p, $s));
    }

    public function testConvertsObjectsToArraysWhenToArrayInterface()
    {
        $o = $this->getMockBuilder('Guzzle\Common\ToArrayInterface')
            ->setMethods(array('toArray'))
            ->getMockForAbstractClass();
        $o->expects($this->once())
            ->method('toArray')
            ->will($this->returnValue(array(
                'foo' => 'bar'
            )));
        $p = new Parameter(array(
            'name'       => 'test',
            'type'       => 'object',
            'properties' => array(
                'foo' => array('required' => 'true')
            )
        ));
        $this->assertTrue($this->validator->validate($p, $o));
    }

    public function testMergesValidationErrorsInPropertiesWithParent()
    {
        $p = new Parameter(array(
            'name'       => 'foo',
            'type'       => 'object',
            'properties' => array(
                'bar'   => array('type' => 'string', 'required' => true, 'description' => 'This is what it does'),
                'test'  => array('type' => 'string', 'minLength' => 2, 'maxLength' => 5),
                'test2' => array('type' => 'string', 'minLength' => 2, 'maxLength' => 2),
                'test3' => array('type' => 'integer', 'minimum' => 100),
                'test4' => array('type' => 'integer', 'maximum' => 10),
                'test5' => array('type' => 'array', 'maxItems' => 2),
                'test6' => array('type' => 'string', 'enum' => array('a', 'bc')),
                'test7' => array('type' => 'string', 'pattern' => '/[0-9]+/'),
                'test8' => array('type' => 'number'),
                'baz' => array(
                    'type'     => 'array',
                    'minItems' => 2,
                    'required' => true,
                    "items"    => array("type" => "string")
                )
            )
        ));

        $value = array(
            'test' => 'a',
            'test2' => 'abc',
            'baz' => array(false),
            'test3' => 10,
            'test4' => 100,
            'test5' => array(1, 3, 4),
            'test6' => 'Foo',
            'test7' => 'abc',
            'test8' => 'abc'
        );

        $this->assertFalse($this->validator->validate($p, $value));
        $this->assertEquals(array (
            '[foo][bar] is a required string: This is what it does',
            '[foo][baz] must contain 2 or more elements',
            '[foo][baz][0] must be of type string',
            '[foo][test2] length must be less than or equal to 2',
            '[foo][test3] must be greater than or equal to 100',
            '[foo][test4] must be less than or equal to 10',
            '[foo][test5] must contain 2 or fewer elements',
            '[foo][test6] must be one of "a" or "bc"',
            '[foo][test7] must match the following regular expression: /[0-9]+/',
            '[foo][test8] must be of type number',
            '[foo][test] length must be greater than or equal to 2',
        ), $this->validator->getErrors());
    }

    public function testHandlesNullValuesInArraysWithDefaults()
    {
        $p = new Parameter(array(
            'name'       => 'foo',
            'type'       => 'object',
            'required'   => true,
            'properties' => array(
                'bar' => array(
                    'type' => 'object',
                    'required' => true,
                    'properties' => array(
                        'foo' => array('default' => 'hi')
                    )
                )
            )
        ));
        $value = array();
        $this->assertTrue($this->validator->validate($p, $value));
        $this->assertEquals(array('bar' => array('foo' => 'hi')), $value);
    }

    public function testFailsWhenNullValuesInArraysWithNoDefaults()
    {
        $p = new Parameter(array(
            'name'       => 'foo',
            'type'       => 'object',
            'required'   => true,
            'properties' => array(
                'bar' => array(
                    'type' => 'object',
                    'required' => true,
                    'properties' => array('foo' => array('type' => 'string'))
                )
            )
        ));
        $value = array();
        $this->assertFalse($this->validator->validate($p, $value));
        $this->assertEquals(array('[foo][bar] is a required object'), $this->validator->getErrors());
    }

    public function testChecksTypes()
    {
        $p = new SchemaValidator();
        $r = new \ReflectionMethod($p, 'determineType');
        $r->setAccessible(true);
        $this->assertEquals('any', $r->invoke($p, 'any', 'hello'));
        $this->assertEquals(false, $r->invoke($p, 'foo', 'foo'));
        $this->assertEquals('string', $r->invoke($p, 'string', 'hello'));
        $this->assertEquals(false, $r->invoke($p, 'string', false));
        $this->assertEquals('integer', $r->invoke($p, 'integer', 1));
        $this->assertEquals(false, $r->invoke($p, 'integer', 'abc'));
        $this->assertEquals('numeric', $r->invoke($p, 'numeric', 1));
        $this->assertEquals('numeric', $r->invoke($p, 'numeric', '1'));
        $this->assertEquals('number', $r->invoke($p, 'number', 1));
        $this->assertEquals('number', $r->invoke($p, 'number', '1'));
        $this->assertEquals(false, $r->invoke($p, 'numeric', 'a'));
        $this->assertEquals('boolean', $r->invoke($p, 'boolean', true));
        $this->assertEquals('boolean', $r->invoke($p, 'boolean', false));
        $this->assertEquals(false, $r->invoke($p, 'boolean', 'false'));
        $this->assertEquals('null', $r->invoke($p, 'null', null));
        $this->assertEquals(false, $r->invoke($p, 'null', 'abc'));
        $this->assertEquals('array', $r->invoke($p, 'array', array()));
        $this->assertEquals(false, $r->invoke($p, 'array', 'foo'));
    }

    public function testValidatesFalseAdditionalProperties()
    {
        $param = new Parameter(array(
            'name'      => 'foo',
            'type'      => 'object',
            'properties' => array('bar' => array('type' => 'string')),
            'additionalProperties' => false
        ));
        $value = array('test' => '123');
        $this->assertFalse($this->validator->validate($param, $value));
        $this->assertEquals(array('[foo][test] is not an allowed property'), $this->validator->getErrors());
        $value = array('bar' => '123');
        $this->assertTrue($this->validator->validate($param, $value));
    }

    public function testAllowsUndefinedAdditionalProperties()
    {
        $param = new Parameter(array(
            'name'      => 'foo',
            'type'      => 'object',
            'properties' => array('bar' => array('type' => 'string'))
        ));
        $value = array('test' => '123');
        $this->assertTrue($this->validator->validate($param, $value));
    }

    public function testValidatesAdditionalProperties()
    {
        $param = new Parameter(array(
            'name'      => 'foo',
            'type'      => 'object',
            'properties' => array('bar' => array('type' => 'string')),
            'additionalProperties' => array('type' => 'integer')
        ));
        $value = array('test' => 'foo');
        $this->assertFalse($this->validator->validate($param, $value));
        $this->assertEquals(array('[foo][test] must be of type integer'), $this->validator->getErrors());
    }

    public function testValidatesAdditionalPropertiesThatArrayArrays()
    {
        $param = new Parameter(array(
            'name' => 'foo',
            'type' => 'object',
            'additionalProperties' => array(
                'type'  => 'array',
                'items' => array('type' => 'string')
            )
        ));
        $value = array('test' => array(true));
        $this->assertFalse($this->validator->validate($param, $value));
        $this->assertEquals(array('[foo][test][0] must be of type string'), $this->validator->getErrors());
    }

    public function testIntegersCastToStringWhenTypeMismatch()
    {
        $param = new Parameter(array('name' => 'test', 'type' => 'string'));
        $value = 12;
        $this->assertTrue($this->validator->validate($param, $value));
        $this->assertEquals('12', $value);
    }

    public function testRequiredMessageIncludesType()
    {
        $param = new Parameter(array('name' => 'test', 'type' => array('string', 'boolean'), 'required' => true));
        $value = null;
        $this->assertFalse($this->validator->validate($param, $value));
        $this->assertEquals(array('[test] is a required string or boolean'), $this->validator->getErrors());
    }

    protected function getComplexParam()
    {
        return new Parameter(array(
            'name'     => 'Foo',
            'type'     => 'array',
            'required' => true,
            'min'      => 1,
            'items'    => array(
                'type'       => 'object',
                'properties' => array(
                    'Baz' => array(
                        'type'    => 'string',
                    ),
                    'Bar' => array(
                        'required' => true,
                        'type'     => 'boolean',
                        'default'  => true
                    )
                )
            )
        ));
    }
}
