<?php

namespace Guzzle\Tests\Service;

/**
 * @covers Guzzle\Service\AbstractConfigLoader
 */
class AbstractConfigLoaderTest extends \Guzzle\Tests\GuzzleTestCase
{
    /** @var \Guzzle\Service\AbstractConfigLoader */
    protected $loader;

    /** @var array Any files that need to be deleted on tear down */
    protected $cleanup = array();

    public function setUp()
    {
        $this->loader = $this->getMockBuilder('Guzzle\Service\AbstractConfigLoader')
            ->setMethods(array('build'))
            ->getMockForAbstractClass();
    }

    public function tearDown()
    {
        foreach ($this->cleanup as $file) {
            unlink($file);
        }
    }

    /**
     * @expectedException \Guzzle\Common\Exception\InvalidArgumentException
     */
    public function testOnlyLoadsSupportedTypes()
    {
        $this->loader->load(new \stdClass());
    }

    /**
     * @expectedException \Guzzle\Common\Exception\InvalidArgumentException
     * @expectedExceptionMessage Unable to open fooooooo.json
     */
    public function testFileMustBeReadable()
    {
        $this->loader->load('fooooooo.json');
    }

    /**
     * @expectedException \Guzzle\Common\Exception\InvalidArgumentException
     * @expectedExceptionMessage Unknown file extension
     */
    public function testMustBeSupportedExtension()
    {
        $this->loader->load(dirname(__DIR__) . '/TestData/FileBody.txt');
    }

    /**
     * @expectedException \Guzzle\Common\Exception\RuntimeException
     * @expectedExceptionMessage Error loading JSON data from
     */
    public function testJsonMustBeValue()
    {
        $filename = tempnam(sys_get_temp_dir(), 'json') . '.json';
        file_put_contents($filename, '{/{./{}foo');
        $this->cleanup[] = $filename;
        $this->loader->load($filename);
    }

    /**
     * @expectedException \Guzzle\Common\Exception\InvalidArgumentException
     * @expectedExceptionMessage PHP files must return an array
     */
    public function testPhpFilesMustReturnAnArray()
    {
        $filename = tempnam(sys_get_temp_dir(), 'php') . '.php';
        file_put_contents($filename, '<?php $fdr = false;');
        $this->cleanup[] = $filename;
        $this->loader->load($filename);
    }

    public function testLoadsPhpFileIncludes()
    {
        $filename = tempnam(sys_get_temp_dir(), 'php') . '.php';
        file_put_contents($filename, '<?php return array("foo" => "bar");');
        $this->cleanup[] = $filename;
        $this->loader->expects($this->exactly(1))->method('build')->will($this->returnArgument(0));
        $config = $this->loader->load($filename);
        $this->assertEquals(array('foo' => 'bar'), $config);
    }

    public function testCanCreateFromJson()
    {
        $file = dirname(__DIR__) . '/TestData/services/json1.json';
        // The build method will just return the config data
        $this->loader->expects($this->exactly(1))->method('build')->will($this->returnArgument(0));
        $data = $this->loader->load($file);
        // Ensure that the config files were merged using the includes directives
        $this->assertArrayHasKey('includes', $data);
        $this->assertArrayHasKey('services', $data);
        $this->assertInternalType('array', $data['services']['foo']);
        $this->assertInternalType('array', $data['services']['abstract']);
        $this->assertInternalType('array', $data['services']['mock']);
        $this->assertEquals('bar', $data['services']['foo']['params']['baz']);
    }

    public function testUsesAliases()
    {
        $file = dirname(__DIR__) . '/TestData/services/json1.json';
        $this->loader->addAlias('foo', $file);
        // The build method will just return the config data
        $this->loader->expects($this->exactly(1))->method('build')->will($this->returnArgument(0));
        $data = $this->loader->load('foo');
        $this->assertEquals('bar', $data['services']['foo']['params']['baz']);
    }

    /**
     * @expectedException \Guzzle\Common\Exception\InvalidArgumentException
     * @expectedExceptionMessage Unable to open foo.json
     */
    public function testCanRemoveAliases()
    {
        $file = dirname(__DIR__) . '/TestData/services/json1.json';
        $this->loader->addAlias('foo.json', $file);
        $this->loader->removeAlias('foo.json');
        $this->loader->load('foo.json');
    }

    public function testCanLoadArraysWithIncludes()
    {
        $file = dirname(__DIR__) . '/TestData/services/json1.json';
        $config = array('includes' => array($file));
        // The build method will just return the config data
        $this->loader->expects($this->exactly(1))->method('build')->will($this->returnArgument(0));
        $data = $this->loader->load($config);
        $this->assertEquals('bar', $data['services']['foo']['params']['baz']);
    }

    public function testDoesNotEnterInfiniteLoop()
    {
        $prefix = $file = dirname(__DIR__) . '/TestData/description';
        $this->loader->load("{$prefix}/baz.json");
        $this->assertCount(4, $this->readAttribute($this->loader, 'loadedFiles'));
        // Ensure that the internal list of loaded files is reset
        $this->loader->load("{$prefix}/../test_service2.json");
        $this->assertCount(1, $this->readAttribute($this->loader, 'loadedFiles'));
        // Ensure that previously loaded files will be reloaded when starting fresh
        $this->loader->load("{$prefix}/baz.json");
        $this->assertCount(4, $this->readAttribute($this->loader, 'loadedFiles'));
    }
}
