<?php
/**
 * phpDocumentor Collection Test
 * 
 * PHP version 5.3
 *
 * @author    Mike van Riel <mike.vanriel@naenius.com>
 * @copyright 2010-2011 Mike van Riel / Naenius. (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */

namespace phpDocumentor\Reflection\DocBlock\Type;

use phpDocumentor\Reflection\DocBlock\Context;

/**
 * Test class for \phpDocumentor\Reflection\DocBlock\Type\Collection
 * 
 * @covers phpDocumentor\Reflection\DocBlock\Type\Collection
 *
 * @author    Mike van Riel <mike.vanriel@naenius.com>
 * @copyright 2010-2011 Mike van Riel / Naenius. (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */
class CollectionTest extends \PHPUnit_Framework_TestCase
{
    /**
     * @covers phpDocumentor\Reflection\DocBlock\Type\Collection::__construct
     * @covers phpDocumentor\Reflection\DocBlock\Type\Collection::getContext
     * 
     * @return void
     */
    public function testConstruct()
    {
        $collection = new Collection();
        $this->assertCount(0, $collection);
        $this->assertEquals('', $collection->getContext()->getNamespace());
        $this->assertCount(0, $collection->getContext()->getNamespaceAliases());
    }

    /**
     * @covers phpDocumentor\Reflection\DocBlock\Type\Collection::__construct
     * 
     * @return void
     */
    public function testConstructWithTypes()
    {
        $collection = new Collection(array('integer', 'string'));
        $this->assertCount(2, $collection);
    }

    /**
     * @covers phpDocumentor\Reflection\DocBlock\Type\Collection::__construct
     * 
     * @return void
     */
    public function testConstructWithNamespace()
    {
        $collection = new Collection(array(), new Context('\My\Space'));
        $this->assertEquals('My\Space', $collection->getContext()->getNamespace());

        $collection = new Collection(array(), new Context('My\Space'));
        $this->assertEquals('My\Space', $collection->getContext()->getNamespace());

        $collection = new Collection(array(), null);
        $this->assertEquals('', $collection->getContext()->getNamespace());
    }

    /**
     * @covers phpDocumentor\Reflection\DocBlock\Type\Collection::__construct
     * 
     * @return void
     */
    public function testConstructWithNamespaceAliases()
    {
        $fixture = array('a' => 'b');
        $collection = new Collection(array(), new Context(null, $fixture));
        $this->assertEquals(
            array('a' => '\b'),
            $collection->getContext()->getNamespaceAliases()
        );
    }

    /**
     * @param string $fixture
     * @param array  $expected
     *
     * @dataProvider provideTypesToExpand
     * @covers phpDocumentor\Reflection\DocBlock\Type\Collection::add
     * 
     * @return void
     */
    public function testAdd($fixture, $expected)
    {
        $collection = new Collection(
            array(),
            new Context('\My\Space', array('Alias' => '\My\Space\Aliasing'))
        );
        $collection->add($fixture);

        $this->assertSame($expected, $collection->getArrayCopy());
    }

    /**
     * @param string $fixture
     * @param array  $expected
     *
     * @dataProvider provideTypesToExpandWithoutNamespace
     * @covers phpDocumentor\Reflection\DocBlock\Type\Collection::add
     * 
     * @return void
     */
    public function testAddWithoutNamespace($fixture, $expected)
    {
        $collection = new Collection(
            array(),
            new Context(null, array('Alias' => '\My\Space\Aliasing'))
        );
        $collection->add($fixture);

        $this->assertSame($expected, $collection->getArrayCopy());
    }

    /**
     * @covers phpDocumentor\Reflection\DocBlock\Type\Collection::add
     * @expectedException InvalidArgumentException
     * 
     * @return void
     */
    public function testAddWithInvalidArgument()
    {
        $collection = new Collection();
        $collection->add(array());
    }

    /**
     * Returns the types and their expected values to test the retrieval of
     * types.
     *
     * @param string $method    Name of the method consuming this data provider.
     * @param string $namespace Name of the namespace to user as basis.
     *
     * @return string[]
     */
    public function provideTypesToExpand($method, $namespace = '\My\Space\\')
    {
        return array(
            array('', array()),
            array(' ', array()),
            array('int', array('int')),
            array('int ', array('int')),
            array('string', array('string')),
            array('DocBlock', array($namespace.'DocBlock')),
            array('DocBlock[]', array($namespace.'DocBlock[]')),
            array(' DocBlock ', array($namespace.'DocBlock')),
            array('\My\Space\DocBlock', array('\My\Space\DocBlock')),
            array('Alias\DocBlock', array('\My\Space\Aliasing\DocBlock')),
            array(
                'DocBlock|Tag',
                array($namespace .'DocBlock', $namespace .'Tag')
            ),
            array(
                'DocBlock|null',
                array($namespace.'DocBlock', 'null')
            ),
            array(
                '\My\Space\DocBlock|Tag',
                array('\My\Space\DocBlock', $namespace.'Tag')
            ),
            array(
                'DocBlock[]|null',
                array($namespace.'DocBlock[]', 'null')
            ),
            array(
                'DocBlock[]|int[]',
                array($namespace.'DocBlock[]', 'int[]')
            ),
        );
    }

    /**
     * Returns the types and their expected values to test the retrieval of
     * types when no namespace is available.
     *
     * @param string $method Name of the method consuming this data provider.
     *
     * @return string[]
     */
    public function provideTypesToExpandWithoutNamespace($method)
    {
        return $this->provideTypesToExpand($method, '\\');
    }
}
