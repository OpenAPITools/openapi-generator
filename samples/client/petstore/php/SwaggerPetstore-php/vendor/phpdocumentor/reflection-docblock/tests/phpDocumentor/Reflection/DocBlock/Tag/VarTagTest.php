<?php
/**
 * phpDocumentor Var Tag Test
 * 
 * PHP version 5.3
 *
 * @author    Daniel O'Connor <daniel.oconnor@gmail.com>
 * @copyright 2010-2011 Mike van Riel / Naenius. (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */

namespace phpDocumentor\Reflection\DocBlock\Tag;

/**
 * Test class for \phpDocumentor\Reflection\DocBlock\Tag\VarTag
 *
 * @author    Daniel O'Connor <daniel.oconnor@gmail.com>
 * @copyright 2010-2011 Mike van Riel / Naenius. (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */
class VarTagTest extends \PHPUnit_Framework_TestCase
{
    /**
     * Test that the \phpDocumentor\Reflection\DocBlock\Tag\VarTag can
     * understand the @var doc block.
     *
     * @param string $type
     * @param string $content
     * @param string $exType
     * @param string $exVariable
     * @param string $exDescription
     *
     * @covers \phpDocumentor\Reflection\DocBlock\Tag\VarTag
     * @dataProvider provideDataForConstuctor
     *
     * @return void
     */
    public function testConstructorParesInputsIntoCorrectFields(
        $type,
        $content,
        $exType,
        $exVariable,
        $exDescription
    ) {
        $tag = new VarTag($type, $content);

        $this->assertEquals($type, $tag->getName());
        $this->assertEquals($exType, $tag->getType());
        $this->assertEquals($exVariable, $tag->getVariableName());
        $this->assertEquals($exDescription, $tag->getDescription());
    }

    /**
     * Data provider for testConstructorParesInputsIntoCorrectFields
     *
     * @return array
     */
    public function provideDataForConstuctor()
    {
        // $type, $content, $exType, $exVariable, $exDescription
        return array(
            array(
                'var',
                'int',
                'int',
                '',
                ''
            ),
            array(
                'var',
                'int $bob',
                'int',
                '$bob',
                ''
            ),
            array(
                'var',
                'int $bob Number of bobs',
                'int',
                '$bob',
                'Number of bobs'
            ),
            array(
                'var',
                '',
                '',
                '',
                ''
            ),
        );
    }
}
