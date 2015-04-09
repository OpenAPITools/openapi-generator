<?php
/**
 * phpDocumentor Description Test
 *
 * PHP Version 5.3
 *
 * @author    Vasil Rangelov <boen.robot@gmail.com>
 * @copyright 2010-2011 Mike van Riel / Naenius. (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */

namespace phpDocumentor\Reflection\DocBlock;

/**
 * Test class for \phpDocumentor\Reflection\DocBlock\Description
 *
 * @author    Vasil Rangelov <boen.robot@gmail.com>
 * @copyright 2010-2011 Mike van Riel / Naenius. (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */
class DescriptionTest extends \PHPUnit_Framework_TestCase
{
    public function testConstruct()
    {
        $fixture = <<<LONGDESC
This is text for a description.
LONGDESC;
        $object = new Description($fixture);
        $this->assertSame($fixture, $object->getContents());

        $parsedContents = $object->getParsedContents();
        $this->assertCount(1, $parsedContents);
        $this->assertSame($fixture, $parsedContents[0]);
    }

    public function testInlineTagParsing()
    {
        $fixture = <<<LONGDESC
This is text for a {@link http://phpdoc.org/ description} that uses inline
tags.
LONGDESC;
        $object = new Description($fixture);
        $this->assertSame($fixture, $object->getContents());

        $parsedContents = $object->getParsedContents();
        $this->assertCount(3, $parsedContents);
        $this->assertSame('This is text for a ', $parsedContents[0]);
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag\LinkTag',
            $parsedContents[1]
        );
        $this->assertSame(
            ' that uses inline
tags.',
            $parsedContents[2]
        );
    }

    public function testInlineTagAtStartParsing()
    {
        $fixture = <<<LONGDESC
{@link http://phpdoc.org/ This} is text for a description that uses inline
tags.
LONGDESC;
        $object = new Description($fixture);
        $this->assertSame($fixture, $object->getContents());

        $parsedContents = $object->getParsedContents();
        $this->assertCount(3, $parsedContents);

        $this->assertSame('', $parsedContents[0]);
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag\LinkTag',
            $parsedContents[1]
        );
        $this->assertSame(
            ' is text for a description that uses inline
tags.',
            $parsedContents[2]
        );
    }

    public function testNestedInlineTagParsing()
    {
        $fixture = <<<LONGDESC
This is text for a description with {@internal inline tag with
{@link http://phpdoc.org another inline tag} in it}.
LONGDESC;
        $object = new Description($fixture);
        $this->assertSame($fixture, $object->getContents());

        $parsedContents = $object->getParsedContents();
        $this->assertCount(3, $parsedContents);

        $this->assertSame(
            'This is text for a description with ',
            $parsedContents[0]
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $parsedContents[1]
        );
        $this->assertSame('.', $parsedContents[2]);

        $parsedDescription = $parsedContents[1]->getParsedDescription();
        $this->assertCount(3, $parsedDescription);
        $this->assertSame("inline tag with\n", $parsedDescription[0]);
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag\LinkTag',
            $parsedDescription[1]
        );
        $this->assertSame(' in it', $parsedDescription[2]);
    }

    public function testLiteralOpeningDelimiter()
    {
        $fixture = <<<LONGDESC
This is text for a description containing { that is literal.
LONGDESC;
        $object = new Description($fixture);
        $this->assertSame($fixture, $object->getContents());

        $parsedContents = $object->getParsedContents();
        $this->assertCount(1, $parsedContents);
        $this->assertSame($fixture, $parsedContents[0]);
    }

    public function testNestedLiteralOpeningDelimiter()
    {
        $fixture = <<<LONGDESC
This is text for a description containing {@internal inline tag that has { that
is literal}.
LONGDESC;
        $object = new Description($fixture);
        $this->assertSame($fixture, $object->getContents());

        $parsedContents = $object->getParsedContents();
        $this->assertCount(3, $parsedContents);
        $this->assertSame(
            'This is text for a description containing ',
            $parsedContents[0]
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $parsedContents[1]
        );
        $this->assertSame('.', $parsedContents[2]);

        $this->assertSame(
            array('inline tag that has { that
is literal'),
            $parsedContents[1]->getParsedDescription()
        );
    }

    public function testLiteralClosingDelimiter()
    {
        $fixture = <<<LONGDESC
This is text for a description with {} that is not a tag.
LONGDESC;
        $object = new Description($fixture);
        $this->assertSame($fixture, $object->getContents());

        $parsedContents = $object->getParsedContents();
        $this->assertCount(1, $parsedContents);
        $this->assertSame(
            'This is text for a description with } that is not a tag.',
            $parsedContents[0]
        );
    }

    public function testNestedLiteralClosingDelimiter()
    {
        $fixture = <<<LONGDESC
This is text for a description with {@internal inline tag with {} that is not an
inline tag}.
LONGDESC;
        $object = new Description($fixture);
        $this->assertSame($fixture, $object->getContents());

        $parsedContents = $object->getParsedContents();
        $this->assertCount(3, $parsedContents);
        $this->assertSame(
            'This is text for a description with ',
            $parsedContents[0]
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $parsedContents[1]
        );
        $this->assertSame('.', $parsedContents[2]);

        $this->assertSame(
            array('inline tag with } that is not an
inline tag'),
            $parsedContents[1]->getParsedDescription()
        );
    }

    public function testInlineTagEscapingSequence()
    {
        $fixture = <<<LONGDESC
This is text for a description with literal {{@}link}.
LONGDESC;
        $object = new Description($fixture);
        $this->assertSame($fixture, $object->getContents());

        $parsedContents = $object->getParsedContents();
        $this->assertCount(1, $parsedContents);
        $this->assertSame(
            'This is text for a description with literal {@link}.',
            $parsedContents[0]
        );
    }

    public function testNestedInlineTagEscapingSequence()
    {
        $fixture = <<<LONGDESC
This is text for a description with an {@internal inline tag with literal
{{@}link{} in it}.
LONGDESC;
        $object = new Description($fixture);
        $this->assertSame($fixture, $object->getContents());

        $parsedContents = $object->getParsedContents();
        $this->assertCount(3, $parsedContents);
        $this->assertSame(
            'This is text for a description with an ',
            $parsedContents[0]
        );
        $this->assertInstanceOf(
            __NAMESPACE__ . '\Tag',
            $parsedContents[1]
        );
        $this->assertSame('.', $parsedContents[2]);

        $this->assertSame(
            array('inline tag with literal
{@link} in it'),
            $parsedContents[1]->getParsedDescription()
        );
    }
}
