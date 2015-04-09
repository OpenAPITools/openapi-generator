<?php
/**
 * phpDocumentor Since Tag Test
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
 * Test class for \phpDocumentor\Reflection\DocBlock\Tag\SinceTag
 *
 * @author    Vasil Rangelov <boen.robot@gmail.com>
 * @copyright 2010-2011 Mike van Riel / Naenius. (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */
class SinceTagTest extends \PHPUnit_Framework_TestCase
{
    /**
     * Test that the \phpDocumentor\Reflection\DocBlock\Tag\LinkTag can create
     * a link for the @since doc block.
     *
     * @param string $type
     * @param string $content
     * @param string $exContent
     * @param string $exDescription
     * @param string $exVersion
     *
     * @covers \phpDocumentor\Reflection\DocBlock\Tag\SinceTag
     * @dataProvider provideDataForConstuctor
     *
     * @return void
     */
    public function testConstructorParesInputsIntoCorrectFields(
        $type,
        $content,
        $exContent,
        $exDescription,
        $exVersion
    ) {
        $tag = new SinceTag($type, $content);

        $this->assertEquals($type, $tag->getName());
        $this->assertEquals($exContent, $tag->getContent());
        $this->assertEquals($exDescription, $tag->getDescription());
        $this->assertEquals($exVersion, $tag->getVersion());
    }

    /**
     * Data provider for testConstructorParesInputsIntoCorrectFields
     *
     * @return array
     */
    public function provideDataForConstuctor()
    {
        // $type, $content, $exContent, $exDescription, $exVersion
        return array(
            array(
                'since',
                '1.0 First release.',
                '1.0 First release.',
                'First release.',
                '1.0'
            ),
            array(
                'since',
                "1.0\nFirst release.",
                "1.0\nFirst release.",
                'First release.',
                '1.0'
            ),
            array(
                'since',
                "1.0\nFirst\nrelease.",
                "1.0\nFirst\nrelease.",
                "First\nrelease.",
                '1.0'
            ),
            array(
                'since',
                'Unfinished release',
                'Unfinished release',
                'Unfinished release',
                ''
            ),
            array(
                'since',
                '1.0',
                '1.0',
                '',
                '1.0'
            ),
            array(
                'since',
                'GIT: $Id$',
                'GIT: $Id$',
                '',
                'GIT: $Id$'
            ),
            array(
                'since',
                'GIT: $Id$ Dev build',
                'GIT: $Id$ Dev build',
                'Dev build',
                'GIT: $Id$'
            )
        );
    }
}
