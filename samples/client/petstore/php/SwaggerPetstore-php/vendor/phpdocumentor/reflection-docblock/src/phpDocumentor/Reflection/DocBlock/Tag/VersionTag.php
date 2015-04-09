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
 * Reflection class for a @version tag in a Docblock.
 *
 * @author  Vasil Rangelov <boen.robot@gmail.com>
 * @license http://www.opensource.org/licenses/mit-license.php MIT
 * @link    http://phpdoc.org
 */
class VersionTag extends Tag
{
    /**
     * PCRE regular expression matching a version vector.
     * Assumes the "x" modifier.
     */
    const REGEX_VECTOR = '(?:
        # Normal release vectors.
        \d\S*
        |
        # VCS version vectors. Per PHPCS, they are expected to
        # follow the form of the VCS name, followed by ":", followed
        # by the version vector itself.
        # By convention, popular VCSes like CVS, SVN and GIT use "$"
        # around the actual version vector.
        [^\s\:]+\:\s*\$[^\$]+\$
    )';

    /** @var string The version vector. */
    protected $version = '';
    
    public function getContent()
    {
        if (null === $this->content) {
            $this->content = "{$this->version} {$this->description}";
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
                # The version vector
                (' . self::REGEX_VECTOR . ')
                \s*
                # The description
                (.+)?
            $/sux',
            $this->description,
            $matches
        )) {
            $this->version = $matches[1];
            $this->setDescription(isset($matches[2]) ? $matches[2] : '');
            $this->content = $content;
        }

        return $this;
    }

    /**
     * Gets the version section of the tag.
     *
     * @return string The version section of the tag.
     */
    public function getVersion()
    {
        return $this->version;
    }
    
    /**
     * Sets the version section of the tag.
     * 
     * @param string $version The new version section of the tag.
     *     An invalid value will set an empty string.
     * 
     * @return $this
     */
    public function setVersion($version)
    {
        $this->version
            = preg_match('/^' . self::REGEX_VECTOR . '$/ux', $version)
            ? $version
            : '';

        $this->content = null;
        return $this;
    }
}
