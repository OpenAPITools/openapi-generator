<?php

namespace Guzzle\Tests\Service\Resource;

use Guzzle\Service\Resource\Model;
use Guzzle\Service\Description\Parameter;
use Guzzle\Common\Collection;

/**
 * @covers Guzzle\Service\Resource\Model
 */
class ModelTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testOwnsStructure()
    {
        $param = new Parameter(array('type' => 'object'));
        $model = new Model(array('foo' => 'bar'), $param);
        $this->assertSame($param, $model->getStructure());
        $this->assertEquals('bar', $model->get('foo'));
        $this->assertEquals('bar', $model['foo']);
    }

    public function testCanBeUsedWithoutStructure()
    {
        $model = new Model(array(
            'Foo' => 'baz',
            'Bar' => array(
                'Boo' => 'Bam'
            )
        ));
        $transform = function ($key, $value) {
            return ($value && is_array($value)) ? new Collection($value) : $value;
        };
        $model = $model->map($transform);
        $this->assertInstanceOf('Guzzle\Common\Collection', $model->getPath('Bar'));
    }

    public function testAllowsFiltering()
    {
        $model = new Model(array(
            'Foo' => 'baz',
            'Bar' => 'a'
        ));
        $model = $model->filter(function ($i, $v) {
            return $v[0] == 'a';
        });
        $this->assertEquals(array('Bar' => 'a'), $model->toArray());
    }

    public function testDoesNotIncludeEmptyStructureInString()
    {
        $model = new Model(array('Foo' => 'baz'));
        $str = (string) $model;
        $this->assertContains('Debug output of model', $str);
        $this->assertNotContains('Model structure', $str);
    }

    public function testDoesIncludeModelStructureInString()
    {
        $model = new Model(array('Foo' => 'baz'), new Parameter(array('name' => 'Foo')));
        $str = (string) $model;
        $this->assertContains('Debug output of Foo model', $str);
        $this->assertContains('Model structure', $str);
    }
}
