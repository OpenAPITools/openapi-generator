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

namespace phpDocumentor\Reflection\DocBlock;

use phpDocumentor\Reflection\DocBlock;
use phpDocumentor\Reflection\DocBlock\Context;

/**
 * Test class for \phpDocumentor\Reflection\DocBlock\Tag\VarTag
 *
 * @author    Daniel O'Connor <daniel.oconnor@gmail.com>
 * @copyright 2010-2011 Mike van Riel / Naenius. (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */
class TagTest extends \PHPUnit_Framework_TestCase
{
    
    /**
     * @expectedException \InvalidArgumentException
     * 
     * @return void
     */
    public function testInvalidTagLine()
    {
        Tag::createInstance('Invalid tag line');
    }

    /**
     * @covers \phpDocumentor\Reflection\DocBlock\Tag::registerTagHandler
     * 
     * @return void
     */
    public function testTagHandlerUnregistration()
    {
        $currentHandler = __NAMESPACE__ . '\Tag\VarTag';
        $tagPreUnreg = Tag::createInstance('@var mixed');
        $this->assertInstanceOf(
            $currentHandler,
            $tagPreUnreg
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $tagPreUnreg
        );

        Tag::registerTagHandler('var', null);

        $tagPostUnreg = Tag::createInstance('@var mixed');
        $this->assertNotInstanceOf(
            $currentHandler,
            $tagPostUnreg
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $tagPostUnreg
        );

        Tag::registerTagHandler('var', $currentHandler);
    }

    /**
     * @covers \phpDocumentor\Reflection\DocBlock\Tag::registerTagHandler
     * 
     * @return void
     */
    public function testTagHandlerCorrectRegistration()
    {
        if (0 == ini_get('allow_url_include')) {
            $this->markTestSkipped('"data" URIs for includes are required.');
        }
        $currentHandler = __NAMESPACE__ . '\Tag\VarTag';
        $tagPreReg = Tag::createInstance('@var mixed');
        $this->assertInstanceOf(
            $currentHandler,
            $tagPreReg
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $tagPreReg
        );

        include 'data:text/plain;base64,'. base64_encode(
<<<TAG_HANDLER
<?php
    class MyTagHandler extends \phpDocumentor\Reflection\DocBlock\Tag {}
TAG_HANDLER
        );

        $this->assertTrue(Tag::registerTagHandler('var', '\MyTagHandler'));

        $tagPostReg = Tag::createInstance('@var mixed');
        $this->assertNotInstanceOf(
            $currentHandler,
            $tagPostReg
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $tagPostReg
        );
        $this->assertInstanceOf(
            '\MyTagHandler',
            $tagPostReg
        );

        $this->assertTrue(Tag::registerTagHandler('var', $currentHandler));
    }
    
    /**
     * @depends testTagHandlerCorrectRegistration
     * @covers \phpDocumentor\Reflection\DocBlock\Tag::registerTagHandler
     * @covers \phpDocumentor\Reflection\DocBlock\Tag::createInstance
     * 
     * @return void
     */
    public function testNamespacedTagHandlerCorrectRegistration()
    {
        $tagPreReg = Tag::createInstance('@T something');
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $tagPreReg
        );
        $this->assertNotInstanceOf(
            '\MyTagHandler',
            $tagPreReg
        );

        $this->assertTrue(
            Tag::registerTagHandler('\MyNamespace\MyTag', '\MyTagHandler')
        );

        $tagPostReg = Tag::createInstance(
            '@T something',
            new DocBlock(
                '',
                new Context('', array('T' => '\MyNamespace\MyTag'))
            )
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $tagPostReg
        );
        $this->assertInstanceOf(
            '\MyTagHandler',
            $tagPostReg
        );

        $this->assertTrue(
            Tag::registerTagHandler('\MyNamespace\MyTag', null)
        );
    }
    
    /**
     * @depends testTagHandlerCorrectRegistration
     * @covers \phpDocumentor\Reflection\DocBlock\Tag::registerTagHandler
     * @covers \phpDocumentor\Reflection\DocBlock\Tag::createInstance
     * 
     * @return void
     */
    public function testNamespacedTagHandlerIncorrectRegistration()
    {
        $tagPreReg = Tag::createInstance('@T something');
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $tagPreReg
        );
        $this->assertNotInstanceOf(
            '\MyTagHandler',
            $tagPreReg
        );

        $this->assertFalse(
            Tag::registerTagHandler('MyNamespace\MyTag', '\MyTagHandler')
        );

        $tagPostReg = Tag::createInstance(
            '@T something',
            new DocBlock(
                '',
                new Context('', array('T' => '\MyNamespace\MyTag'))
            )
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $tagPostReg
        );
        $this->assertNotInstanceOf(
            '\MyTagHandler',
            $tagPostReg
        );
    }

    /**
     * @covers \phpDocumentor\Reflection\DocBlock\Tag::registerTagHandler
     * 
     * @return void
     */
    public function testNonExistentTagHandlerRegistration()
    {
        $currentHandler = __NAMESPACE__ . '\Tag\VarTag';
        $tagPreReg = Tag::createInstance('@var mixed');
        $this->assertInstanceOf(
            $currentHandler,
            $tagPreReg
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $tagPreReg
        );

        $this->assertFalse(Tag::registerTagHandler('var', 'Non existent'));

        $tagPostReg = Tag::createInstance('@var mixed');
        $this->assertInstanceOf(
            $currentHandler,
            $tagPostReg
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $tagPostReg
        );
    }

    /**
     * @covers \phpDocumentor\Reflection\DocBlock\Tag::registerTagHandler
     * 
     * @return void
     */
    public function testIncompatibleTagHandlerRegistration()
    {
        $currentHandler = __NAMESPACE__ . '\Tag\VarTag';
        $tagPreReg = Tag::createInstance('@var mixed');
        $this->assertInstanceOf(
            $currentHandler,
            $tagPreReg
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $tagPreReg
        );

        $this->assertFalse(
            Tag::registerTagHandler('var', __NAMESPACE__ . '\TagTest')
        );

        $tagPostReg = Tag::createInstance('@var mixed');
        $this->assertInstanceOf(
            $currentHandler,
            $tagPostReg
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $tagPostReg
        );
    }

    /**
     * Test that the \phpDocumentor\Reflection\DocBlock\Tag\VarTag can
     * understand the @var doc block.
     *
     * @param string $type
     * @param string $content
     * @param string $exDescription
     *
     * @covers \phpDocumentor\Reflection\DocBlock\Tag
     * @dataProvider provideDataForConstuctor
     *
     * @return void
     */
    public function testConstructorParesInputsIntoCorrectFields(
        $type,
        $content,
        $exDescription
    ) {
        $tag = new Tag($type, $content);

        $this->assertEquals($type, $tag->getName());
        $this->assertEquals($content, $tag->getContent());
        $this->assertEquals($exDescription, $tag->getDescription());
    }

    /**
     * Data provider for testConstructorParesInputsIntoCorrectFields
     *
     * @return array
     */
    public function provideDataForConstuctor()
    {
        // $type, $content, $exDescription
        return array(
            array(
                'unknown',
                'some content',
                'some content',
            ),
            array(
                'unknown',
                '',
                '',
            )
        );
    }
}
