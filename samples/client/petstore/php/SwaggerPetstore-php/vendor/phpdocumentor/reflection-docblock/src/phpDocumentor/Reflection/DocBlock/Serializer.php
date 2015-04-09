<?php
/**
 * phpDocumentor
 *
 * PHP Version 5.3
 *
 * @author    Barry vd. Heuvel <barryvdh@gmail.com>
 * @copyright 2013 Mike van Riel / Naenius (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */

namespace phpDocumentor\Reflection\DocBlock;

use phpDocumentor\Reflection\DocBlock;

/**
 * Serializes a DocBlock instance.
 *
 * @author  Barry vd. Heuvel <barryvdh@gmail.com>
 * @license http://www.opensource.org/licenses/mit-license.php MIT
 * @link    http://phpdoc.org
 */
class Serializer
{

    /** @var string The string to indent the comment with. */
    protected $indentString = ' ';

    /** @var int The number of times the indent string is repeated. */
    protected $indent = 0;

    /** @var bool Whether to indent the first line. */
    protected $isFirstLineIndented = true;

    /** @var int|null The max length of a line. */
    protected $lineLength = null;

    /**
     * Create a Serializer instance.
     *
     * @param int      $indent          The number of times the indent string is
     *     repeated.
     * @param string   $indentString    The string to indent the comment with.
     * @param bool     $indentFirstLine Whether to indent the first line.
     * @param int|null $lineLength      The max length of a line or NULL to
     *     disable line wrapping.
     */
    public function __construct(
        $indent = 0,
        $indentString = ' ',
        $indentFirstLine = true,
        $lineLength = null
    ) {
        $this->setIndentationString($indentString);
        $this->setIndent($indent);
        $this->setIsFirstLineIndented($indentFirstLine);
        $this->setLineLength($lineLength);
    }

    /**
     * Sets the string to indent comments with.
     * 
     * @param string $indentationString The string to indent comments with.
     * 
     * @return $this This serializer object.
     */
    public function setIndentationString($indentString)
    {
        $this->indentString = (string)$indentString;
        return $this;
    }

    /**
     * Gets the string to indent comments with.
     * 
     * @return string The indent string.
     */
    public function getIndentationString()
    {
        return $this->indentString;
    }

    /**
     * Sets the number of indents.
     * 
     * @param int $indent The number of times the indent string is repeated.
     * 
     * @return $this This serializer object.
     */
    public function setIndent($indent)
    {
        $this->indent = (int)$indent;
        return $this;
    }

    /**
     * Gets the number of indents.
     * 
     * @return int The number of times the indent string is repeated.
     */
    public function getIndent()
    {
        return $this->indent;
    }

    /**
     * Sets whether or not the first line should be indented.
     * 
     * Sets whether or not the first line (the one with the "/**") should be
     * indented.
     * 
     * @param bool $indentFirstLine The new value for this setting.
     * 
     * @return $this This serializer object.
     */
    public function setIsFirstLineIndented($indentFirstLine)
    {
        $this->isFirstLineIndented = (bool)$indentFirstLine;
        return $this;
    }

    /**
     * Gets whether or not the first line should be indented.
     * 
     * @return bool Whether or not the first line should be indented.
     */
    public function isFirstLineIndented()
    {
        return $this->isFirstLineIndented;
    }

    /**
     * Sets the line length.
     * 
     * Sets the length of each line in the serialization. Content will be
     * wrapped within this limit.
     * 
     * @param int|null $lineLength The length of each line. NULL to disable line
     *     wrapping altogether.
     * 
     * @return $this This serializer object.
     */
    public function setLineLength($lineLength)
    {
        $this->lineLength = null === $lineLength ? null : (int)$lineLength;
        return $this;
    }

    /**
     * Gets the line length.
     * 
     * @return int|null The length of each line or NULL if line wrapping is
     *     disabled.
     */
    public function getLineLength()
    {
        return $this->lineLength;
    }

    /**
     * Generate a DocBlock comment.
     *
     * @param DocBlock The DocBlock to serialize.
     * 
     * @return string The serialized doc block.
     */
    public function getDocComment(DocBlock $docblock)
    {
        $indent = str_repeat($this->indentString, $this->indent);
        $firstIndent = $this->isFirstLineIndented ? $indent : '';

        $text = $docblock->getText();
        if ($this->lineLength) {
            //3 === strlen(' * ')
            $wrapLength = $this->lineLength - strlen($indent) - 3;
            $text = wordwrap($text, $wrapLength);
        }
        $text = str_replace("\n", "\n{$indent} * ", $text);

        $comment = "{$firstIndent}/**\n{$indent} * {$text}\n{$indent} *\n";

        /** @var Tag $tag */
        foreach ($docblock->getTags() as $tag) {
            $tagText = (string) $tag;
            if ($this->lineLength) {
                $tagText = wordwrap($tagText, $wrapLength);
            }
            $tagText = str_replace("\n", "\n{$indent} * ", $tagText);

            $comment .= "{$indent} * {$tagText}\n";
        }

        $comment .= $indent . ' */';

        return $comment;
    }
}
