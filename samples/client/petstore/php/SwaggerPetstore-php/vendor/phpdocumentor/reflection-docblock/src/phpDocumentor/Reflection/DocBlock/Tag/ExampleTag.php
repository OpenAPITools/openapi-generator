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
 * Reflection class for a @example tag in a Docblock.
 *
 * @author  Vasil Rangelov <boen.robot@gmail.com>
 * @license http://www.opensource.org/licenses/mit-license.php MIT
 * @link    http://phpdoc.org
 */
class ExampleTag extends SourceTag
{
    /** 
     * @var string Path to a file to use as an example.
     *     May also be an absolute URI.
     */
    protected $filePath = '';

    /**
     * @var bool Whether the file path component represents an URI.
     *     This determines how the file portion appears at {@link getContent()}.
     */
    protected $isURI = false;

    /**
     * {@inheritdoc}
     */
    public function getContent()
    {
        if (null === $this->content) {
            $filePath = '';
            if ($this->isURI) {
                if (false === strpos($this->filePath, ':')) {
                    $filePath = str_replace(
                        '%2F',
                        '/',
                        rawurlencode($this->filePath)
                    );
                } else {
                    $filePath = $this->filePath;
                }
            } else {
                $filePath = '"' . $this->filePath . '"';
            }

            $this->content = $filePath . ' ' . parent::getContent();
        }

        return $this->content;
    }
    /**
     * {@inheritdoc}
     */
    public function setContent($content)
    {
        Tag::setContent($content);
        if (preg_match(
            '/^
                # File component
                (?:
                    # File path in quotes
                    \"([^\"]+)\"
                    |
                    # File URI
                    (\S+)
                )
                # Remaining content (parsed by SourceTag)
                (?:\s+(.*))?
            $/sux',
            $this->description,
            $matches
        )) {
            if ('' !== $matches[1]) {
                $this->setFilePath($matches[1]);
            } else {
                $this->setFileURI($matches[2]);
            }

            if (isset($matches[3])) {
                parent::setContent($matches[3]);
            } else {
                $this->setDescription('');
            }
            $this->content = $content;
        }

        return $this;
    }

    /**
     * Returns the file path.
     *
     * @return string Path to a file to use as an example.
     *     May also be an absolute URI.
     */
    public function getFilePath()
    {
        return $this->filePath;
    }
    
    /**
     * Sets the file path.
     * 
     * @param string $filePath The new file path to use for the example.
     * 
     * @return $this
     */
    public function setFilePath($filePath)
    {
        $this->isURI = false;
        $this->filePath = trim($filePath);

        $this->content = null;
        return $this;
    }
    
    /**
     * Sets the file path as an URI.
     * 
     * This function is equivalent to {@link setFilePath()}, except that it
     * convers an URI to a file path before that.
     * 
     * There is no getFileURI(), as {@link getFilePath()} is compatible.
     * 
     * @param type $uri The new file URI to use as an example.
     */
    public function setFileURI($uri)
    {
        $this->isURI = true;
        if (false === strpos($uri, ':')) {
            //Relative URL
            $this->filePath = rawurldecode(
                str_replace(array('/', '\\'), '%2F', $uri)
            );
        } else {
            //Absolute URL or URI.
            $this->filePath = $uri;
        }

        $this->content = null;
        return $this;
    }
}
