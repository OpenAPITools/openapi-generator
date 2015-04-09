<?php
/**
 * phpDocumentor
 *
 * PHP Version 5.3
 *
 * @author    Vasil Rangelov <boen.robot@gmail.com>
 * @copyright 2010-2011 Mike van Riel / Naenius (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */

namespace phpDocumentor\Reflection\DocBlock\Tag;

use phpDocumentor\Reflection\DocBlock\Tag;

/**
 * Reflection class for a @source tag in a Docblock.
 *
 * @author  Vasil Rangelov <boen.robot@gmail.com>
 * @license http://www.opensource.org/licenses/mit-license.php MIT
 * @link    http://phpdoc.org
 */
class SourceTag extends Tag
{
    /**
     * @var int The starting line, relative to the structural element's
     *     location.
     */
    protected $startingLine = 1;

    /** 
     * @var int|null The number of lines, relative to the starting line. NULL
     *     means "to the end".
     */
    protected $lineCount = null;

    /**
     * {@inheritdoc}
     */
    public function getContent()
    {
        if (null === $this->content) {
            $this->content
                = "{$this->startingLine} {$this->lineCount} {$this->description}";
        }

        return $this->content;
    }

    /**
     * {@inheritdoc}
     */
    public function setContent($content)
    {
        parent::setContent($content);
        if (preg_match(
            '/^
                # Starting line
                ([1-9]\d*)
                \s*
                # Number of lines
                (?:
                    ((?1))
                    \s+
                )?
                # Description
                (.*)
            $/sux',
            $this->description,
            $matches
        )) {
            $this->startingLine = (int)$matches[1];
            if (isset($matches[2]) && '' !== $matches[2]) {
                $this->lineCount = (int)$matches[2];
            }
            $this->setDescription($matches[3]);
            $this->content = $content;
        }

        return $this;
    }

    /**
     * Gets the starting line.
     *
     * @return int The starting line, relative to the structural element's
     *     location.
     */
    public function getStartingLine()
    {
        return $this->startingLine;
    }

    /**
     * Sets the starting line.
     * 
     * @param int $startingLine The new starting line, relative to the
     *     structural element's location.
     * 
     * @return $this
     */
    public function setStartingLine($startingLine)
    {
        $this->startingLine = $startingLine;

        $this->content = null;
        return $this;
    }

    /**
     * Returns the number of lines.
     *
     * @return int|null The number of lines, relative to the starting line. NULL
     *     means "to the end".
     */
    public function getLineCount()
    {
        return $this->lineCount;
    }

    /**
     * Sets the number of lines.
     * 
     * @param int|null $lineCount The new number of lines, relative to the
     *     starting line. NULL means "to the end".
     * 
     * @return $this
     */
    public function setLineCount($lineCount)
    {
        $this->lineCount = $lineCount;

        $this->content = null;
        return $this;
    }
}
