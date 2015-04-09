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

namespace phpDocumentor\Reflection\DocBlock;

/**
 * The location a DocBlock occurs within a file.
 *
 * @author  Vasil Rangelov <boen.robot@gmail.com>
 * @license http://www.opensource.org/licenses/mit-license.php MIT
 * @link    http://phpdoc.org
 */
class Location
{
    /** @var int Line where the DocBlock text starts. */
    protected $lineNumber = 0;

    /** @var int Column where the DocBlock text starts. */
    protected $columnNumber = 0;
    
    public function __construct(
        $lineNumber = 0,
        $columnNumber = 0
    ) {
        $this->setLineNumber($lineNumber)->setColumnNumber($columnNumber);
    }

    /**
     * @return int Line where the DocBlock text starts.
     */
    public function getLineNumber()
    {
        return $this->lineNumber;
    }

    /**
     * 
     * @param type $lineNumber
     * @return $this
     */
    public function setLineNumber($lineNumber)
    {
        $this->lineNumber = (int)$lineNumber;

        return $this;
    }

    /**
     * @return int Column where the DocBlock text starts.
     */
    public function getColumnNumber()
    {
        return $this->columnNumber;
    }

    /**
     * 
     * @param int $columnNumber
     * @return $this
     */
    public function setColumnNumber($columnNumber)
    {
        $this->columnNumber = (int)$columnNumber;

        return $this;
    }
}
