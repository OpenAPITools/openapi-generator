<?php

/*
 * This file is part of the Symfony package.
 *
 * (c) Fabien Potencier <fabien@symfony.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace Symfony\Component\Config\Tests\Resource;

use Symfony\Component\Config\Resource\DirectoryResource;

class DirectoryResourceTest extends \PHPUnit_Framework_TestCase
{
    protected $directory;

    protected function setUp()
    {
        $this->directory = sys_get_temp_dir().'/symfonyDirectoryIterator';
        if (!file_exists($this->directory)) {
            mkdir($this->directory);
        }
        touch($this->directory.'/tmp.xml');
    }

    protected function tearDown()
    {
        if (!is_dir($this->directory)) {
            return;
        }
        $this->removeDirectory($this->directory);
    }

    protected function removeDirectory($directory)
    {
        $iterator = new \RecursiveIteratorIterator(new \RecursiveDirectoryIterator($directory), \RecursiveIteratorIterator::CHILD_FIRST);
        foreach ($iterator as $path) {
            if (preg_match('#[/\\\\]\.\.?$#', $path->__toString())) {
                continue;
            }
            if ($path->isDir()) {
                rmdir($path->__toString());
            } else {
                unlink($path->__toString());
            }
        }
        rmdir($directory);
    }

    public function testGetResource()
    {
        $resource = new DirectoryResource($this->directory);
        $this->assertSame($this->directory, $resource->getResource(), '->getResource() returns the path to the resource');
        $this->assertSame($this->directory, (string) $resource, '->__toString() returns the path to the resource');
    }

    public function testGetPattern()
    {
        $resource = new DirectoryResource('foo', 'bar');
        $this->assertEquals('bar', $resource->getPattern());
    }

    public function testIsFresh()
    {
        $resource = new DirectoryResource($this->directory);
        $this->assertTrue($resource->isFresh(time() + 10), '->isFresh() returns true if the resource has not changed');
        $this->assertFalse($resource->isFresh(time() - 86400), '->isFresh() returns false if the resource has been updated');

        $resource = new DirectoryResource('/____foo/foobar'.rand(1, 999999));
        $this->assertFalse($resource->isFresh(time()), '->isFresh() returns false if the resource does not exist');
    }

    public function testIsFreshUpdateFile()
    {
        $resource = new DirectoryResource($this->directory);
        touch($this->directory.'/tmp.xml', time() + 20);
        $this->assertFalse($resource->isFresh(time() + 10), '->isFresh() returns false if an existing file is modified');
    }

    public function testIsFreshNewFile()
    {
        $resource = new DirectoryResource($this->directory);
        touch($this->directory.'/new.xml', time() + 20);
        $this->assertFalse($resource->isFresh(time() + 10), '->isFresh() returns false if a new file is added');
    }

    public function testIsFreshDeleteFile()
    {
        $resource = new DirectoryResource($this->directory);
        unlink($this->directory.'/tmp.xml');
        $this->assertFalse($resource->isFresh(time()), '->isFresh() returns false if an existing file is removed');
    }

    public function testIsFreshDeleteDirectory()
    {
        $resource = new DirectoryResource($this->directory);
        $this->removeDirectory($this->directory);
        $this->assertFalse($resource->isFresh(time()), '->isFresh() returns false if the whole resource is removed');
    }

    public function testIsFreshCreateFileInSubdirectory()
    {
        $subdirectory = $this->directory.'/subdirectory';
        mkdir($subdirectory);

        $resource = new DirectoryResource($this->directory);
        $this->assertTrue($resource->isFresh(time() + 10), '->isFresh() returns true if an unmodified subdirectory exists');

        touch($subdirectory.'/newfile.xml', time() + 20);
        $this->assertFalse($resource->isFresh(time() + 10), '->isFresh() returns false if a new file in a subdirectory is added');
    }

    public function testIsFreshModifySubdirectory()
    {
        $resource = new DirectoryResource($this->directory);

        $subdirectory = $this->directory.'/subdirectory';
        mkdir($subdirectory);
        touch($subdirectory, time() + 20);

        $this->assertFalse($resource->isFresh(time() + 10), '->isFresh() returns false if a subdirectory is modified (e.g. a file gets deleted)');
    }

    public function testFilterRegexListNoMatch()
    {
        $resource = new DirectoryResource($this->directory, '/\.(foo|xml)$/');

        touch($this->directory.'/new.bar', time() + 20);
        $this->assertTrue($resource->isFresh(time() + 10), '->isFresh() returns true if a new file not matching the filter regex is created');
    }

    public function testFilterRegexListMatch()
    {
        $resource = new DirectoryResource($this->directory, '/\.(foo|xml)$/');

        touch($this->directory.'/new.xml', time() + 20);
        $this->assertFalse($resource->isFresh(time() + 10), '->isFresh() returns false if an new file matching the filter regex is created ');
    }

    public function testSerializeUnserialize()
    {
        $resource = new DirectoryResource($this->directory, '/\.(foo|xml)$/');

        $unserialized = unserialize(serialize($resource));

        $this->assertSame($this->directory, $resource->getResource());
        $this->assertSame('/\.(foo|xml)$/', $resource->getPattern());
    }
}
