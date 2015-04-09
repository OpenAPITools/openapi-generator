<?php
namespace Contrib\Component\File;

/**
 * @covers Contrib\Component\File\Path
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class PathTest extends \PHPUnit_Framework_TestCase
{
    protected function setUp()
    {
        $this->existingFile = __DIR__ . '/existing.txt';
        $this->unreadablePath = __DIR__ . '/unreadable.txt';
        $this->unwritablePath = __DIR__ . '/unwritable.txt';
        $this->unwritableDir  = __DIR__ . '/unwritable.dir';

        $this->object = new Path();
    }

    protected function tearDown()
    {
        $this->rmFile($this->existingFile);
        $this->rmFile($this->unreadablePath);
        $this->rmFile($this->unwritablePath);

        $this->rmDir($this->unwritableDir);
    }

    protected function rmFile($file)
    {
        if (is_file($file)) {
            unlink($file);
        }
    }

    protected function rmDir($dir)
    {
        if (is_dir($dir)) {
            rmdir($dir);
        }
    }

    protected function touchUnreadableFile()
    {
        $this->rmFile($this->unreadablePath);

        touch($this->unreadablePath);
        chmod($this->unreadablePath, 0377);
    }

    protected function touchUnwritableFile()
    {
        $this->rmFile($this->unwritablePath);

        touch($this->unwritablePath);
        chmod($this->unwritablePath, 0577);
    }

    protected function mkdirUnwritableDir()
    {
        $this->rmDir($this->unwritableDir);

        mkdir($this->unwritableDir);
        chmod($this->unwritableDir, 0577);
    }

    // provider

    public function provideRelativePaths()
    {
        return array(
            array(''),
            array('.'),
            array('..'),
        );
    }

    public function proviceAbsolutePaths()
    {
        return array(
            array('/'),
            array('/path/to/somewhere'),
        );
    }

    // isRelativePath()

    /**
     * @test
     * @dataProvider provideRelativePaths
     */
    public function shouldBeRelativePath($path)
    {
        $this->assertTrue($this->object->isRelativePath($path));
    }

    /**
     * @test
     * @dataProvider proviceAbsolutePaths
     */
    public function shouldNotBeRelativePath($path)
    {
        $this->assertFalse($this->object->isRelativePath($path));
    }

    // toAbsolutePath()

    /**
     * @test
     */
    public function shouldNotConvertAbsolutePath()
    {
        $path    = false;
        $rootDir = __DIR__;

        $this->assertFalse($this->object->toAbsolutePath($path, $rootDir));
    }

    /**
     * @test
     * @dataProvider provideRelativePaths
     */
    public function shouldConvertAbsolutePathIfRelativePathGiven($path)
    {
        $rootDir = '/path/to/dir';

        $expected = $rootDir . DIRECTORY_SEPARATOR . $path;

        $this->assertEquals($expected, $this->object->toAbsolutePath($path, $rootDir));
    }

    /**
     * @test
     */
    public function shouldConvertAbsolutePathIfAbsolutePathGiven()
    {
        $rootDir = '/path/to/dir';
        $path    = __DIR__;

        $expected = $path;

        $this->assertEquals($expected, $this->object->toAbsolutePath($path, $rootDir));
    }

    // getRealPath()

    /**
     * @test
     */
    public function shouldNotBeRealPath()
    {
        $path    = false;
        $rootDir = __DIR__;

        $this->assertFalse($this->object->getRealPath($path, $rootDir));
    }

    /**
     * @test
     * @dataProvider provideRelativePaths
     */
    public function shouldGetRealPathIfRelativePathGiven($path)
    {
        $rootDir = __DIR__;

        $expected = realpath($rootDir . DIRECTORY_SEPARATOR . $path);

        $this->assertEquals($expected, $this->object->getRealPath($path, $rootDir));
    }

    /**
     * @test
     */
    public function shouldGetRealPathIfAbsolutePathGiven()
    {
        $path    = __DIR__;
        $rootDir = '';

        $expected = realpath($path);

        $this->assertEquals($expected, $this->object->getRealPath($path, $rootDir));
    }

    // getRealDir()

    /**
     * @test
     */
    public function shouldNotBeRealDir()
    {
        $path    = false;
        $rootDir = __DIR__;

        $this->assertFalse($this->object->getRealDir($path, $rootDir));
    }

    /**
     * @test
     */
    public function shouldGetRealDirIfRelativePathGiven()
    {
        $path    = '';
        $rootDir = __DIR__;

        $expected = realpath($rootDir . DIRECTORY_SEPARATOR . $path);

        $this->assertEquals($expected, $this->object->getRealDir($path, $rootDir));
    }

    /**
     * @test
     */
    public function shouldGetRealDirIfAbsolutePathGiven()
    {
        $path    = __DIR__;
        $rootDir = '';

        $expected = realpath($path . '/..');

        $this->assertEquals($expected, $this->object->getRealDir($path, $rootDir));
    }

    // getRealWritingFilePath()

    /**
     * @test
     */
    public function shouldNotBeRealWritingFilePath()
    {
        $path    = false;
        $rootDir = __DIR__;

        $this->assertFalse($this->object->getRealWritingFilePath($path, $rootDir));
    }

    /**
     * @test
     */
    public function shouldGetRealWritingPathIfRelativePathGiven()
    {
        $path    = 'test.txt';
        $rootDir = __DIR__;

        $expected = $rootDir . DIRECTORY_SEPARATOR . $path;

        $this->assertEquals($expected, $this->object->getRealWritingFilePath($path, $rootDir));
    }

    // isRealPathExist()

    /**
     * @test
     */
    public function shouldNotExistRealPathIfFalseGiven()
    {
        $path = false;

        $this->assertFalse($this->object->isRealPathExist($path));
    }

    /**
     * @test
     */
    public function shouldNotExistRealPath()
    {
        $path = __DIR__ . '/dummy.dir';

        $this->assertFalse($this->object->isRealPathExist($path));
    }

    /**
     * @test
     */
    public function shouldExistRealPath()
    {
        touch($this->existingFile);

        $this->assertTrue($this->object->isRealPathExist($this->existingFile));
    }

    // isRealFileExist()

    /**
     * @test
     */
    public function shouldNotExistRealFile()
    {
        $path    = __DIR__ . '/dummy.file';

        $this->assertFalse($this->object->isRealFileExist($path));
    }

    /**
     * @test
     */
    public function shouldNotExistRealFileIfDirGiven()
    {
        $path    = __DIR__;

        $this->assertFalse($this->object->isRealFileExist($path));
    }

    /**
     * @test
     */
    public function shouldExistRealFile()
    {
        touch($this->existingFile);

        $this->assertTrue($this->object->isRealFileExist($this->existingFile));
    }

    // isRealFileReadable()

    /**
     * @test
     */
    public function shouldNotBeRealFileReadableIfFileNotFound()
    {
        $path    = __DIR__ . '/dummy.file';

        $this->assertFalse($this->object->isRealFileReadable($path));
    }

    /**
     * @test
     */
    public function shouldNotBeRealFileReadableIFFileUnreadable()
    {
        $this->touchUnreadableFile();

        $this->assertFalse($this->object->isRealFileReadable($this->unreadablePath));
    }

    /**
     * @test
     */
    public function shouldBeRealFileReadable()
    {
        touch($this->existingFile);

        $this->assertTrue($this->object->isRealFileReadable($this->existingFile));
    }

    // isRealFileWritable()

    /**
     * @test
     */
    public function shouldNotBeRealFileWritableIfFileNotFound()
    {
        $path    = __DIR__ . '/dummy.file';

        $this->assertFalse($this->object->isRealFileWritable($path));
    }

    /**
     * @test
     */
    public function shouldNotBeRealFileWritableIfFileUnwritable()
    {
        $this->touchUnwritableFile();

        $this->assertFalse($this->object->isRealFileWritable($this->unwritablePath));
    }

    /**
     * @test
     */
    public function shouldBeRealFileWritable()
    {
        touch($this->existingFile);

        $this->assertTrue($this->object->isRealFileWritable($this->existingFile));
    }

    // isRealDirExist()

    /**
     * @test
     */
    public function shouldNotExistRealDir()
    {
        $path = __DIR__ . '/dummy.dir';

        $this->assertFalse($this->object->isRealDirExist($path));
    }

    /**
     * @test
     */
    public function shouldNotExistRealDirIfFileGiven()
    {
        touch($this->existingFile);


        $this->assertFalse($this->object->isRealDirExist($this->existingFile));
    }

    /**
     * @test
     */
    public function shouldExistRealDir()
    {
        $path = __DIR__;

        $this->assertTrue($this->object->isRealDirExist($path));
    }

    // isRealDirWritable()

    /**
     * @test
     */
    public function shouldNotBeRealDirWritableIfDirNotFound()
    {
        $path = __DIR__ . '/dummy.dir';

        $this->assertFalse($this->object->isRealDirWritable($path));
    }

    /**
     * @test
     */
    public function shouldNotBeRealDirWritableIfDirUnwritable()
    {
        $this->mkdirUnwritableDir();

        $this->assertFalse($this->object->isRealDirWritable($this->unwritableDir));
    }

    /**
     * @test
     */
    public function shouldBeRealDirWritable()
    {
        $path = __DIR__;

        $this->assertTrue($this->object->isRealDirWritable($path));
    }
}
