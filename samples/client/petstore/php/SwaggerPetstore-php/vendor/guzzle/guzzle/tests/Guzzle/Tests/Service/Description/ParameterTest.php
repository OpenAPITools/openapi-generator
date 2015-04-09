<?php

namespace Guzzle\Tests\Service\Description;

use Guzzle\Service\Description\Parameter;
use Guzzle\Service\Description\ServiceDescription;

/**
 * @covers Guzzle\Service\Description\Parameter
 */
class ParameterTest extends \Guzzle\Tests\GuzzleTestCase
{
    protected $data = array(
        'name'            => 'foo',
        'type'            => 'bar',
        'required'        => true,
        'default'         => '123',
        'description'     => '456',
        'minLength'       => 2,
        'maxLength'       => 5,
        'location'        => 'body',
        'static'          => 'static!',
        'filters'         => array('trim', 'json_encode')
    );

    public function testCreatesParamFromArray()
    {
        $p = new Parameter($this->data);
        $this->assertEquals('foo', $p->getName());
        $this->assertEquals('bar', $p->getType());
        $this->assertEquals(true, $p->getRequired());
        $this->assertEquals('123', $p->getDefault());
        $this->assertEquals('456', $p->getDescription());
        $this->assertEquals(2, $p->getMinLength());
        $this->assertEquals(5, $p->getMaxLength());
        $this->assertEquals('body', $p->getLocation());
        $this->assertEquals('static!', $p->getStatic());
        $this->assertEquals(array('trim', 'json_encode'), $p->getFilters());
    }

    public function testCanConvertToArray()
    {
        $p = new Parameter($this->data);
        unset($this->data['name']);
        $this->assertEquals($this->data, $p->toArray());
    }

    public function testUsesStatic()
    {
        $d = $this->data;
        $d['default'] = 'booboo';
        $d['static'] = true;
        $p = new Parameter($d);
        $this->assertEquals('booboo', $p->getValue('bar'));
    }

    public function testUsesDefault()
    {
        $d = $this->data;
        $d['default'] = 'foo';
        $d['static'] = null;
        $p = new Parameter($d);
        $this->assertEquals('foo', $p->getValue(null));
    }

    public function testReturnsYourValue()
    {
        $d = $this->data;
        $d['static'] = null;
        $p = new Parameter($d);
        $this->assertEquals('foo', $p->getValue('foo'));
    }

    public function testZeroValueDoesNotCauseDefaultToBeReturned()
    {
        $d = $this->data;
        $d['default'] = '1';
        $d['static'] = null;
        $p = new Parameter($d);
        $this->assertEquals('0', $p->getValue('0'));
    }

    public function testFiltersValues()
    {
        $d = $this->data;
        $d['static'] = null;
        $d['filters'] = 'strtoupper';
        $p = new Parameter($d);
        $this->assertEquals('FOO', $p->filter('foo'));
    }

    public function testConvertsBooleans()
    {
        $p = new Parameter(array('type' => 'boolean'));
        $this->assertEquals(true, $p->filter('true'));
        $this->assertEquals(false, $p->filter('false'));
    }

    public function testUsesArrayByDefaultForFilters()
    {
        $d = $this->data;
        $d['filters'] = null;
        $p = new Parameter($d);
        $this->assertEquals(array(), $p->getFilters());
    }

    public function testAllowsSimpleLocationValue()
    {
        $p = new Parameter(array('name' => 'myname', 'location' => 'foo', 'sentAs' => 'Hello'));
        $this->assertEquals('foo', $p->getLocation());
        $this->assertEquals('Hello', $p->getSentAs());
    }

    public function testParsesTypeValues()
    {
        $p = new Parameter(array('type' => 'foo'));
        $this->assertEquals('foo', $p->getType());
    }

    /**
     * @expectedException InvalidArgumentException
     * @expectedExceptionMessage A [method] value must be specified for each complex filter
     */
    public function testValidatesComplexFilters()
    {
        $p = new Parameter(array('filters' => array(array('args' => 'foo'))));
    }

    public function testCanBuildUpParams()
    {
        $p = new Parameter(array());
        $p->setName('foo')
            ->setDescription('c')
            ->setFilters(array('d'))
            ->setLocation('e')
            ->setSentAs('f')
            ->setMaxLength(1)
            ->setMinLength(1)
            ->setMinimum(2)
            ->setMaximum(2)
            ->setMinItems(3)
            ->setMaxItems(3)
            ->setRequired(true)
            ->setStatic(true)
            ->setDefault('h')
            ->setType('i');

        $p->addFilter('foo');

        $this->assertEquals('foo', $p->getName());
        $this->assertEquals('h', $p->getDefault());
        $this->assertEquals('c', $p->getDescription());
        $this->assertEquals(array('d', 'foo'), $p->getFilters());
        $this->assertEquals('e', $p->getLocation());
        $this->assertEquals('f', $p->getSentAs());
        $this->assertEquals(1, $p->getMaxLength());
        $this->assertEquals(1, $p->getMinLength());
        $this->assertEquals(2, $p->getMaximum());
        $this->assertEquals(2, $p->getMinimum());
        $this->assertEquals(3, $p->getMaxItems());
        $this->assertEquals(3, $p->getMinItems());
        $this->assertEquals(true, $p->getRequired());
        $this->assertEquals(true, $p->getStatic());
        $this->assertEquals('i', $p->getType());
    }

    public function testAllowsNestedShape()
    {
        $command = $this->getServiceBuilder()->get('mock')->getCommand('mock_command')->getOperation();
        $param = new Parameter(array(
            'parent'     => $command,
            'name'       => 'foo',
            'type'       => 'object',
            'location'   => 'query',
            'properties' => array(
                'foo' => array(
                    'type'      => 'object',
                    'required'  => true,
                    'properties' => array(
                        'baz' => array(
                            'name' => 'baz',
                            'type' => 'bool',
                        )
                    )
                ),
                'bar' => array(
                    'name'    => 'bar',
                    'default' => '123'
                )
            )
        ));

        $this->assertSame($command, $param->getParent());
        $this->assertNotEmpty($param->getProperties());
        $this->assertInstanceOf('Guzzle\Service\Description\Parameter', $param->getProperty('foo'));
        $this->assertSame($param, $param->getProperty('foo')->getParent());
        $this->assertSame($param->getProperty('foo'), $param->getProperty('foo')->getProperty('baz')->getParent());
        $this->assertInstanceOf('Guzzle\Service\Description\Parameter', $param->getProperty('bar'));
        $this->assertSame($param, $param->getProperty('bar')->getParent());

        $array = $param->toArray();
        $this->assertInternalType('array', $array['properties']);
        $this->assertArrayHasKey('foo', $array['properties']);
        $this->assertArrayHasKey('bar', $array['properties']);
    }

    public function testAllowsComplexFilters()
    {
        $that = $this;
        $param = new Parameter(array());
        $param->setFilters(array(array('method' => function ($a, $b, $c, $d) use ($that, $param) {
            $that->assertEquals('test', $a);
            $that->assertEquals('my_value!', $b);
            $that->assertEquals('bar', $c);
            $that->assertSame($param, $d);
            return 'abc' . $b;
        }, 'args' => array('test', '@value', 'bar', '@api'))));
        $this->assertEquals('abcmy_value!', $param->filter('my_value!'));
    }

    public function testCanChangeParentOfNestedParameter()
    {
        $param1 = new Parameter(array('name' => 'parent'));
        $param2 = new Parameter(array('name' => 'child'));
        $param2->setParent($param1);
        $this->assertSame($param1, $param2->getParent());
    }

    public function testCanRemoveFromNestedStructure()
    {
        $param1 = new Parameter(array('name' => 'parent'));
        $param2 = new Parameter(array('name' => 'child'));
        $param1->addProperty($param2);
        $this->assertSame($param1, $param2->getParent());
        $this->assertSame($param2, $param1->getProperty('child'));

        // Remove a single child from the structure
        $param1->removeProperty('child');
        $this->assertNull($param1->getProperty('child'));
        // Remove the entire structure
        $param1->addProperty($param2);
        $param1->removeProperty('child');
        $this->assertNull($param1->getProperty('child'));
    }

    public function testAddsAdditionalProperties()
    {
        $p = new Parameter(array(
            'type' => 'object',
            'additionalProperties' => array('type' => 'string')
        ));
        $this->assertInstanceOf('Guzzle\Service\Description\Parameter', $p->getAdditionalProperties());
        $this->assertNull($p->getAdditionalProperties()->getAdditionalProperties());
        $p = new Parameter(array('type' => 'object'));
        $this->assertTrue($p->getAdditionalProperties());
    }

    public function testAddsItems()
    {
        $p = new Parameter(array(
            'type'  => 'array',
            'items' => array('type' => 'string')
        ));
        $this->assertInstanceOf('Guzzle\Service\Description\Parameter', $p->getItems());
        $out = $p->toArray();
        $this->assertEquals('array', $out['type']);
        $this->assertInternalType('array', $out['items']);
    }

    public function testHasExtraProperties()
    {
        $p = new Parameter();
        $this->assertEquals(array(), $p->getData());
        $p->setData(array('foo' => 'bar'));
        $this->assertEquals('bar', $p->getData('foo'));
        $p->setData('baz', 'boo');
        $this->assertEquals(array('foo' => 'bar', 'baz' => 'boo'), $p->getData());
    }

    public function testCanRetrieveKnownPropertiesUsingDataMethod()
    {
        $p = new Parameter();
        $this->assertEquals(null, $p->getData('foo'));
        $p->setName('test');
        $this->assertEquals('test', $p->getData('name'));
    }

    public function testHasInstanceOf()
    {
        $p = new Parameter();
        $this->assertNull($p->getInstanceOf());
        $p->setInstanceOf('Foo');
        $this->assertEquals('Foo', $p->getInstanceOf());
    }

    public function testHasPattern()
    {
        $p = new Parameter();
        $this->assertNull($p->getPattern());
        $p->setPattern('/[0-9]+/');
        $this->assertEquals('/[0-9]+/', $p->getPattern());
    }

    public function testHasEnum()
    {
        $p = new Parameter();
        $this->assertNull($p->getEnum());
        $p->setEnum(array('foo', 'bar'));
        $this->assertEquals(array('foo', 'bar'), $p->getEnum());
    }

    public function testSerializesItems()
    {
        $p = new Parameter(array(
            'type'  => 'object',
            'additionalProperties' => array('type' => 'string')
        ));
        $this->assertEquals(array(
            'type'  => 'object',
            'additionalProperties' => array('type' => 'string')
        ), $p->toArray());
    }

    public function testResolvesRefKeysRecursively()
    {
        $description = new ServiceDescription(array(
            'models' => array(
                'JarJar' => array('type' => 'string', 'default' => 'Mesa address tha senate!'),
                'Anakin' => array('type' => 'array', 'items' => array('$ref' => 'JarJar'))
            )
        ));
        $p = new Parameter(array('$ref' => 'Anakin', 'description' => 'added'), $description);
        $this->assertEquals(array(
            'type' => 'array',
            'items' => array('type' => 'string', 'default' => 'Mesa address tha senate!'),
            'description' => 'added'
        ), $p->toArray());
    }

    public function testResolvesExtendsRecursively()
    {
        $jarJar = array('type' => 'string', 'default' => 'Mesa address tha senate!', 'description' => 'a');
        $anakin = array('type' => 'array', 'items' => array('extends' => 'JarJar', 'description' => 'b'));
        $description = new ServiceDescription(array(
            'models' => array('JarJar' => $jarJar, 'Anakin' => $anakin)
        ));
        // Description attribute will be updated, and format added
        $p = new Parameter(array('extends' => 'Anakin', 'format' => 'date'), $description);
        $this->assertEquals(array(
            'type'  => 'array',
            'format' => 'date',
            'items' => array(
                'type'    => 'string',
                'default' => 'Mesa address tha senate!',
                'description' => 'b'
            )
        ), $p->toArray());
    }

    public function testHasKeyMethod()
    {
        $p = new Parameter(array('name' => 'foo', 'sentAs' => 'bar'));
        $this->assertEquals('bar', $p->getWireName());
        $p->setSentAs(null);
        $this->assertEquals('foo', $p->getWireName());
    }

    public function testIncludesNameInToArrayWhenItemsAttributeHasName()
    {
        $p = new Parameter(array(
            'type' => 'array',
            'name' => 'Abc',
            'items' => array(
                'name' => 'Foo',
                'type' => 'object'
            )
        ));
        $result = $p->toArray();
        $this->assertEquals(array(
            'type' => 'array',
            'items' => array(
                'name' => 'Foo',
                'type' => 'object',
                'additionalProperties' => true
            )
        ), $result);
    }

    public function dateTimeProvider()
    {
        $d = 'October 13, 2012 16:15:46 UTC';

        return array(
            array($d, 'date-time', '2012-10-13T16:15:46Z'),
            array($d, 'date', '2012-10-13'),
            array($d, 'timestamp', strtotime($d)),
            array(new \DateTime($d), 'timestamp', strtotime($d))
        );
    }

    /**
     * @dataProvider dateTimeProvider
     */
    public function testAppliesFormat($d, $format, $result)
    {
        $p = new Parameter();
        $p->setFormat($format);
        $this->assertEquals($format, $p->getFormat());
        $this->assertEquals($result, $p->filter($d));
    }
}
