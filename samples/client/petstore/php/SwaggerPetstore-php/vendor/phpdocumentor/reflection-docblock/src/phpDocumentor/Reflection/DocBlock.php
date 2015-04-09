<?php
/**
 * phpDocumentor
 *
 * PHP Version 5.3
 *
 * @author    Mike van Riel <mike.vanriel@naenius.com>
 * @copyright 2010-2011 Mike van Riel / Naenius (http://www.naenius.com)
 * @license   http://www.opensource.org/licenses/mit-license.php MIT
 * @link      http://phpdoc.org
 */

namespace phpDocumentor\Reflection;

use phpDocumentor\Reflection\DocBlock\Tag;
use phpDocumentor\Reflection\DocBlock\Context;
use phpDocumentor\Reflection\DocBlock\Location;

/**
 * Parses the DocBlock for any structure.
 *
 * @author  Mike van Riel <mike.vanriel@naenius.com>
 * @license http://www.opensource.org/licenses/mit-license.php MIT
 * @link    http://phpdoc.org
 */
class DocBlock implements \Reflector
{
    /** @var string The opening line for this docblock. */
    protected $short_description = '';

    /**
     * @var DocBlock\Description The actual
     *     description for this docblock.
     */
    protected $long_description = null;

    /**
     * @var Tag[] An array containing all
     *     the tags in this docblock; except inline.
     */
    protected $tags = array();

    /** @var Context Information about the context of this DocBlock. */
    protected $context = null;

    /** @var Location Information about the location of this DocBlock. */
    protected $location = null;

    /** @var bool Is this DocBlock (the start of) a template? */
    protected $isTemplateStart = false;

    /** @var bool Does this DocBlock signify the end of a DocBlock template? */
    protected $isTemplateEnd = false;

    /**
     * Parses the given docblock and populates the member fields.
     *
     * The constructor may also receive namespace information such as the
     * current namespace and aliases. This information is used by some tags
     * (e.g. @return, @param, etc.) to turn a relative Type into a FQCN.
     *
     * @param \Reflector|string $docblock A docblock comment (including
     *     asterisks) or reflector supporting the getDocComment method.
     * @param Context           $context  The context in which the DocBlock
     *     occurs.
     * @param Location          $location The location within the file that this
     *     DocBlock occurs in.
     *
     * @throws \InvalidArgumentException if the given argument does not have the
     *     getDocComment method.
     */
    public function __construct(
        $docblock,
        Context $context = null,
        Location $location = null
    ) {
        if (is_object($docblock)) {
            if (!method_exists($docblock, 'getDocComment')) {
                throw new \InvalidArgumentException(
                    'Invalid object passed; the given reflector must support '
                    . 'the getDocComment method'
                );
            }

            $docblock = $docblock->getDocComment();
        }

        $docblock = $this->cleanInput($docblock);

        list($templateMarker, $short, $long, $tags) = $this->splitDocBlock($docblock);
        $this->isTemplateStart = $templateMarker === '#@+';
        $this->isTemplateEnd = $templateMarker === '#@-';
        $this->short_description = $short;
        $this->long_description = new DocBlock\Description($long, $this);
        $this->parseTags($tags);

        $this->context  = $context;
        $this->location = $location;
    }

    /**
     * Strips the asterisks from the DocBlock comment.
     *
     * @param string $comment String containing the comment text.
     *
     * @return string
     */
    protected function cleanInput($comment)
    {
        $comment = trim(
            preg_replace(
                '#[ \t]*(?:\/\*\*|\*\/|\*)?[ \t]{0,1}(.*)?#u',
                '$1',
                $comment
            )
        );

        // reg ex above is not able to remove */ from a single line docblock
        if (substr($comment, -2) == '*/') {
            $comment = trim(substr($comment, 0, -2));
        }

        // normalize strings
        $comment = str_replace(array("\r\n", "\r"), "\n", $comment);

        return $comment;
    }

    /**
     * Splits the DocBlock into a template marker, summary, description and block of tags.
     *
     * @param string $comment Comment to split into the sub-parts.
     *
     * @author Richard van Velzen (@_richardJ) Special thanks to Richard for the regex responsible for the split.
     * @author Mike van Riel <me@mikevanriel.com> for extending the regex with template marker support.
     *
     * @return string[] containing the template marker (if any), summary, description and a string containing the tags.
     */
    protected function splitDocBlock($comment)
    {
        // Performance improvement cheat: if the first character is an @ then only tags are in this DocBlock. This
        // method does not split tags so we return this verbatim as the fourth result (tags). This saves us the
        // performance impact of running a regular expression
        if (strpos($comment, '@') === 0) {
            return array('', '', '', $comment);
        }

        // clears all extra horizontal whitespace from the line endings to prevent parsing issues
        $comment = preg_replace('/\h*$/Sum', '', $comment);

        /*
         * Splits the docblock into a template marker, short description, long description and tags section
         *
         * - The template marker is empty, #@+ or #@- if the DocBlock starts with either of those (a newline may
         *   occur after it and will be stripped).
         * - The short description is started from the first character until a dot is encountered followed by a
         *   newline OR two consecutive newlines (horizontal whitespace is taken into account to consider spacing
         *   errors). This is optional.
         * - The long description, any character until a new line is encountered followed by an @ and word
         *   characters (a tag). This is optional.
         * - Tags; the remaining characters
         *
         * Big thanks to RichardJ for contributing this Regular Expression
         */
        preg_match(
            '/
            \A
            # 1. Extract the template marker
            (?:(\#\@\+|\#\@\-)\n?)?

            # 2. Extract the summary
            (?:
              (?! @\pL ) # The summary may not start with an @
              (
                [^\n.]+
                (?:
                  (?! \. \n | \n{2} )     # End summary upon a dot followed by newline or two newlines
                  [\n.] (?! [ \t]* @\pL ) # End summary when an @ is found as first character on a new line
                  [^\n.]+                 # Include anything else
                )*
                \.?
              )?
            )

            # 3. Extract the description
            (?:
              \s*        # Some form of whitespace _must_ precede a description because a summary must be there
              (?! @\pL ) # The description may not start with an @
              (
                [^\n]+
                (?: \n+
                  (?! [ \t]* @\pL ) # End description when an @ is found as first character on a new line
                  [^\n]+            # Include anything else
                )*
              )
            )?

            # 4. Extract the tags (anything that follows)
            (\s+ [\s\S]*)? # everything that follows
            /ux',
            $comment,
            $matches
        );
        array_shift($matches);

        while (count($matches) < 4) {
            $matches[] = '';
        }

        return $matches;
    }

    /**
     * Creates the tag objects.
     *
     * @param string $tags Tag block to parse.
     *
     * @return void
     */
    protected function parseTags($tags)
    {
        $result = array();
        $tags = trim($tags);
        if ('' !== $tags) {
            if ('@' !== $tags[0]) {
                throw new \LogicException(
                    'A tag block started with text instead of an actual tag,'
                    . ' this makes the tag block invalid: ' . $tags
                );
            }
            foreach (explode("\n", $tags) as $tag_line) {
                if (isset($tag_line[0]) && ($tag_line[0] === '@')) {
                    $result[] = $tag_line;
                } else {
                    $result[count($result) - 1] .= "\n" . $tag_line;
                }
            }

            // create proper Tag objects
            foreach ($result as $key => $tag_line) {
                $result[$key] = Tag::createInstance(trim($tag_line), $this);
            }
        }

        $this->tags = $result;
    }

    /**
     * Gets the text portion of the doc block.
     * 
     * Gets the text portion (short and long description combined) of the doc
     * block.
     * 
     * @return string The text portion of the doc block.
     */
    public function getText()
    {
        $short = $this->getShortDescription();
        $long = $this->getLongDescription()->getContents();

        if ($long) {
            return "{$short}\n\n{$long}";
        } else {
            return $short;
        }
    }

    /**
     * Set the text portion of the doc block.
     * 
     * Sets the text portion (short and long description combined) of the doc
     * block.
     *
     * @param string $docblock The new text portion of the doc block.
     * 
     * @return $this This doc block.
     */
    public function setText($comment)
    {
        list(,$short, $long) = $this->splitDocBlock($comment);
        $this->short_description = $short;
        $this->long_description = new DocBlock\Description($long, $this);
        return $this;
    }
    /**
     * Returns the opening line or also known as short description.
     *
     * @return string
     */
    public function getShortDescription()
    {
        return $this->short_description;
    }

    /**
     * Returns the full description or also known as long description.
     *
     * @return DocBlock\Description
     */
    public function getLongDescription()
    {
        return $this->long_description;
    }

    /**
     * Returns whether this DocBlock is the start of a Template section.
     *
     * A Docblock may serve as template for a series of subsequent DocBlocks. This is indicated by a special marker
     * (`#@+`) that is appended directly after the opening `/**` of a DocBlock.
     *
     * An example of such an opening is:
     *
     * ```
     * /**#@+
     *  * My DocBlock
     *  * /
     * ```
     *
     * The description and tags (not the summary!) are copied onto all subsequent DocBlocks and also applied to all
     * elements that follow until another DocBlock is found that contains the closing marker (`#@-`).
     *
     * @see self::isTemplateEnd() for the check whether a closing marker was provided.
     *
     * @return boolean
     */
    public function isTemplateStart()
    {
        return $this->isTemplateStart;
    }

    /**
     * Returns whether this DocBlock is the end of a Template section.
     *
     * @see self::isTemplateStart() for a more complete description of the Docblock Template functionality.
     *
     * @return boolean
     */
    public function isTemplateEnd()
    {
        return $this->isTemplateEnd;
    }

    /**
     * Returns the current context.
     *
     * @return Context
     */
    public function getContext()
    {
        return $this->context;
    }

    /**
     * Returns the current location.
     *
     * @return Location
     */
    public function getLocation()
    {
        return $this->location;
    }

    /**
     * Returns the tags for this DocBlock.
     *
     * @return Tag[]
     */
    public function getTags()
    {
        return $this->tags;
    }

    /**
     * Returns an array of tags matching the given name. If no tags are found
     * an empty array is returned.
     *
     * @param string $name String to search by.
     *
     * @return Tag[]
     */
    public function getTagsByName($name)
    {
        $result = array();

        /** @var Tag $tag */
        foreach ($this->getTags() as $tag) {
            if ($tag->getName() != $name) {
                continue;
            }

            $result[] = $tag;
        }

        return $result;
    }

    /**
     * Checks if a tag of a certain type is present in this DocBlock.
     *
     * @param string $name Tag name to check for.
     *
     * @return bool
     */
    public function hasTag($name)
    {
        /** @var Tag $tag */
        foreach ($this->getTags() as $tag) {
            if ($tag->getName() == $name) {
                return true;
            }
        }

        return false;
    }

    /**
     * Appends a tag at the end of the list of tags.
     *
     * @param Tag $tag The tag to add.
     *
     * @return Tag The newly added tag.
     *
     * @throws \LogicException When the tag belongs to a different DocBlock.
     */
    public function appendTag(Tag $tag)
    {
        if (null === $tag->getDocBlock()) {
            $tag->setDocBlock($this);
        }

        if ($tag->getDocBlock() === $this) {
            $this->tags[] = $tag;
        } else {
            throw new \LogicException(
                'This tag belongs to a different DocBlock object.'
            );
        }

        return $tag;
    }


    /**
     * Builds a string representation of this object.
     *
     * @todo determine the exact format as used by PHP Reflection and
     *     implement it.
     *
     * @return string
     * @codeCoverageIgnore Not yet implemented
     */
    public static function export()
    {
        throw new \Exception('Not yet implemented');
    }

    /**
     * Returns the exported information (we should use the export static method
     * BUT this throws an exception at this point).
     *
     * @return string
     * @codeCoverageIgnore Not yet implemented
     */
    public function __toString()
    {
        return 'Not yet implemented';
    }
}
