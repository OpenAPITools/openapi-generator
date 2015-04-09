<?php
/**
 * phpDocumentor Example Tag Test
 * 
 * PHP version 5.3
 *
 * @author    Vasil Rangelov <boen.robot@gmail.com>
 * @copyright 2010-2011 Mike van Riel / Naenius. (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */

namespace phpDocumentor\Reflection\DocBlock\Tag;

/**
 * Test class for \phpDocumentor\Reflection\DocBlock\Tag\ExampleTag
 *
 * @author    Vasil Rangelov <boen.robot@gmail.com>
 * @copyright 2010-2011 Mike van Riel / Naenius. (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */
class ExampleTagTest extends \PHPUnit_Framework_TestCase
{
    /**
     * Test that the \phpDocumentor\Reflection\DocBlock\Tag\SourceTag can
     * understand the @source DocBlock.
     *
     * @param string $type
     * @param string $content
     * @param string $exContent
     * @param string $exStartingLine
     * @param string $exLineCount
     * @param string $exFilepath
     *
     * @covers \phpDocumentor\Reflection\DocBlock\Tag\ExampleTag
     * @dataProvider provideDataForConstuctor
     *
     * @return void
     */
    public function testConstructorParesInputsIntoCorrectFields(
        $type,
        $content,
        $exContent,
        $exDescription,
        $exStartingLine,
        $exLineCount,
        $exFilePath
    ) {
        $tag = new ExampleTag($type, $content);

        $this->assertEquals($type, $tag->getName());
        $this->assertEquals($exContent, $tag->getContent());
        $this->assertEquals($exDescription, $tag->getDescription());
        $this->assertEquals($exStartingLine, $tag->getStartingLine());
        $this->assertEquals($exLineCount, $tag->getLineCount());
        $this->assertEquals($exFilePath, $tag->getFilePath());
    }

    /**
     * Data provider for testConstructorParesInputsIntoCorrectFields
     *
     * @return array
     */
    public function provideDataForConstuctor()
    {
        // $type,
        // $content,
        // $exContent,
        // $exDescription,
        // $exStartingLine,
        // $exLineCount,
        // $exFilePath
        return array(
            array(
                'example',
                'file.php',
                'file.php',
                '',
                1,
                null,
                'file.php'
            ),
            array(
                'example',
                'Testing comments',
                'Testing comments',
                'comments',
                1,
                null,
                'Testing'
            ),
            array(
                'example',
                'file.php 2 Testing',
                'file.php 2 Testing',
                'Testing',
                2,
                null,
                'file.php'
            ),
            array(
                'example',
                'file.php 2 3 Testing comments',
                'file.php 2 3 Testing comments',
                'Testing comments',
                2,
                3,
                'file.php'
            ),
            array(
                'example',
                'file.php 2 -1 Testing comments',
                'file.php 2 -1 Testing comments',
                '-1 Testing comments',
                2,
                null,
                'file.php'
            ),
            array(
                'example',
                'file.php -1 1 Testing comments',
                'file.php -1 1 Testing comments',
                '-1 1 Testing comments',
                1,
                null,
                'file.php'
            ),
            array(
                'example',
                '"file with spaces.php" Testing comments',
                '"file with spaces.php" Testing comments',
                'Testing comments',
                1,
                null,
                'file with spaces.php'
            ),
            array(
                'example',
                '"file with spaces.php" 2 Testing comments',
                '"file with spaces.php" 2 Testing comments',
                'Testing comments',
                2,
                null,
                'file with spaces.php'
            ),
            array(
                'example',
                '"file with spaces.php" 2 3 Testing comments',
                '"file with spaces.php" 2 3 Testing comments',
                'Testing comments',
                2,
                3,
                'file with spaces.php'
            ),
            array(
                'example',
                '"file with spaces.php" 2 -3 Testing comments',
                '"file with spaces.php" 2 -3 Testing comments',
                '-3 Testing comments',
                2,
                null,
                'file with spaces.php'
            ),
            array(
                'example',
                '"file with spaces.php" -2 3 Testing comments',
                '"file with spaces.php" -2 3 Testing comments',
                '-2 3 Testing comments',
                1,
                null,
                'file with spaces.php'
            ),
            array(
                'example',
                'file%20with%20spaces.php Testing comments',
                'file%20with%20spaces.php Testing comments',
                'Testing comments',
                1,
                null,
                'file with spaces.php'
            ),
            array(
                'example',
                'folder/file%20with%20spaces.php Testing comments',
                'folder/file%20with%20spaces.php Testing comments',
                'Testing comments',
                1,
                null,
                'folder/file with spaces.php'
            ),
            array(
                'example',
                'http://example.com/file%20with%20spaces.php Testing comments',
                'http://example.com/file%20with%20spaces.php Testing comments',
                'Testing comments',
                1,
                null,
                'http://example.com/file%20with%20spaces.php'
            )
        );
    }
}
