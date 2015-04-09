<?php
/**
 * phpDocumentor Link Tag Test
 * 
 * PHP version 5.3
 *
 * @author    Ben Selby <benmatselby@gmail.com>
 * @copyright 2010-2011 Mike van Riel / Naenius. (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */

namespace phpDocumentor\Reflection\DocBlock\Tag;

/**
 * Test class for \phpDocumentor\Reflection\DocBlock\Tag\LinkTag
 *
 * @author    Ben Selby <benmatselby@gmail.com>
 * @copyright 2010-2011 Mike van Riel / Naenius. (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */
class LinkTagTest extends \PHPUnit_Framework_TestCase
{
    /**
     * Test that the \phpDocumentor\Reflection\DocBlock\Tag\LinkTag can create
     * a link for the @link doc block.
     *
     * @param string $type
     * @param string $content
     * @param string $exContent
     * @param string $exDescription
     * @param string $exLink
     *
     * @covers \phpDocumentor\Reflection\DocBlock\Tag\LinkTag
     * @dataProvider provideDataForConstuctor
     *
     * @return void
     */
    public function testConstructorParesInputsIntoCorrectFields(
        $type,
        $content,
        $exContent,
        $exDescription,
        $exLink
    ) {
        $tag = new LinkTag($type, $content);

        $this->assertEquals($type, $tag->getName());
        $this->assertEquals($exContent, $tag->getContent());
        $this->assertEquals($exDescription, $tag->getDescription());
        $this->assertEquals($exLink, $tag->getLink());
    }

    /**
     * Data provider for testConstructorParesInputsIntoCorrectFields
     *
     * @return array
     */
    public function provideDataForConstuctor()
    {
        // $type, $content, $exContent, $exDescription, $exLink
        return array(
            array(
                'link',
                'http://www.phpdoc.org/',
                'http://www.phpdoc.org/',
                'http://www.phpdoc.org/',
                'http://www.phpdoc.org/'
            ),
            array(
                'link',
                'http://www.phpdoc.org/ Testing',
                'http://www.phpdoc.org/ Testing',
                'Testing',
                'http://www.phpdoc.org/'
            ),
            array(
                'link',
                'http://www.phpdoc.org/ Testing comments',
                'http://www.phpdoc.org/ Testing comments',
                'Testing comments',
                'http://www.phpdoc.org/'
            ),
        );
    }
}
