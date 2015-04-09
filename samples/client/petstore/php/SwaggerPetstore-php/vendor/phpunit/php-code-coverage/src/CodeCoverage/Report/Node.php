<?php
/*
 * This file is part of the PHP_CodeCoverage package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Base class for nodes in the code coverage information tree.
 *
 * @category   PHP
 * @package    CodeCoverage
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://github.com/sebastianbergmann/php-code-coverage
 * @since      Class available since Release 1.1.0
 */
abstract class PHP_CodeCoverage_Report_Node implements Countable
{
    /**
     * @var string
     */
    protected $name;

    /**
     * @var string
     */
    protected $path;

    /**
     * @var array
     */
    protected $pathArray;

    /**
     * @var PHP_CodeCoverage_Report_Node
     */
    protected $parent;

    /**
     * @var string
     */
    protected $id;

    /**
     * Constructor.
     *
     * @param string                       $name
     * @param PHP_CodeCoverage_Report_Node $parent
     */
    public function __construct($name, PHP_CodeCoverage_Report_Node $parent = null)
    {
        if (substr($name, -1) == '/') {
            $name = substr($name, 0, -1);
        }

        $this->name   = $name;
        $this->parent = $parent;
    }

    /**
     * @return string
     */
    public function getName()
    {
        return $this->name;
    }

    /**
     * @return string
     */
    public function getId()
    {
        if ($this->id === null) {
            $parent = $this->getParent();

            if ($parent === null) {
                $this->id = 'index';
            } else {
                $parentId = $parent->getId();

                if ($parentId == 'index') {
                    $this->id = str_replace(':', '_', $this->name);
                } else {
                    $this->id = $parentId . '/' . $this->name;
                }
            }
        }

        return $this->id;
    }

    /**
     * @return string
     */
    public function getPath()
    {
        if ($this->path === null) {
            if ($this->parent === null || $this->parent->getPath() === null) {
                $this->path = $this->name;
            } else {
                $this->path = $this->parent->getPath() . '/' . $this->name;
            }
        }

        return $this->path;
    }

    /**
     * @return array
     */
    public function getPathAsArray()
    {
        if ($this->pathArray === null) {
            if ($this->parent === null) {
                $this->pathArray = array();
            } else {
                $this->pathArray = $this->parent->getPathAsArray();
            }

            $this->pathArray[] = $this;
        }

        return $this->pathArray;
    }

    /**
     * @return PHP_CodeCoverage_Report_Node
     */
    public function getParent()
    {
        return $this->parent;
    }

    /**
     * Returns the percentage of classes that has been tested.
     *
     * @param  boolean $asString
     * @return integer
     */
    public function getTestedClassesPercent($asString = true)
    {
        return PHP_CodeCoverage_Util::percent(
            $this->getNumTestedClasses(),
            $this->getNumClasses(),
            $asString
        );
    }

    /**
     * Returns the percentage of traits that has been tested.
     *
     * @param  boolean $asString
     * @return integer
     */
    public function getTestedTraitsPercent($asString = true)
    {
        return PHP_CodeCoverage_Util::percent(
            $this->getNumTestedTraits(),
            $this->getNumTraits(),
            $asString
        );
    }

    /**
     * Returns the percentage of traits that has been tested.
     *
     * @param  boolean $asString
     * @return integer
     * @since  Method available since Release 1.2.0
     */
    public function getTestedClassesAndTraitsPercent($asString = true)
    {
        return PHP_CodeCoverage_Util::percent(
            $this->getNumTestedClassesAndTraits(),
            $this->getNumClassesAndTraits(),
            $asString
        );
    }

    /**
     * Returns the percentage of methods that has been tested.
     *
     * @param  boolean $asString
     * @return integer
     */
    public function getTestedMethodsPercent($asString = true)
    {
        return PHP_CodeCoverage_Util::percent(
            $this->getNumTestedMethods(),
            $this->getNumMethods(),
            $asString
        );
    }

    /**
     * Returns the percentage of executed lines.
     *
     * @param  boolean $asString
     * @return integer
     */
    public function getLineExecutedPercent($asString = true)
    {
        return PHP_CodeCoverage_Util::percent(
            $this->getNumExecutedLines(),
            $this->getNumExecutableLines(),
            $asString
        );
    }

    /**
     * Returns the number of classes and traits.
     *
     * @return integer
     * @since  Method available since Release 1.2.0
     */
    public function getNumClassesAndTraits()
    {
        return $this->getNumClasses() + $this->getNumTraits();
    }

    /**
     * Returns the number of tested classes and traits.
     *
     * @return integer
     * @since  Method available since Release 1.2.0
     */
    public function getNumTestedClassesAndTraits()
    {
        return $this->getNumTestedClasses() + $this->getNumTestedTraits();
    }

    /**
     * Returns the classes and traits of this node.
     *
     * @return array
     * @since  Method available since Release 1.2.0
     */
    public function getClassesAndTraits()
    {
        return array_merge($this->getClasses(), $this->getTraits());
    }

    /**
     * Returns the classes of this node.
     *
     * @return array
     */
    abstract public function getClasses();

    /**
     * Returns the traits of this node.
     *
     * @return array
     */
    abstract public function getTraits();

    /**
     * Returns the functions of this node.
     *
     * @return array
     */
    abstract public function getFunctions();

    /**
     * Returns the LOC/CLOC/NCLOC of this node.
     *
     * @return array
     */
    abstract public function getLinesOfCode();

    /**
     * Returns the number of executable lines.
     *
     * @return integer
     */
    abstract public function getNumExecutableLines();

    /**
     * Returns the number of executed lines.
     *
     * @return integer
     */
    abstract public function getNumExecutedLines();

    /**
     * Returns the number of classes.
     *
     * @return integer
     */
    abstract public function getNumClasses();

    /**
     * Returns the number of tested classes.
     *
     * @return integer
     */
    abstract public function getNumTestedClasses();

    /**
     * Returns the number of traits.
     *
     * @return integer
     */
    abstract public function getNumTraits();

    /**
     * Returns the number of tested traits.
     *
     * @return integer
     */
    abstract public function getNumTestedTraits();

    /**
     * Returns the number of methods.
     *
     * @return integer
     */
    abstract public function getNumMethods();

    /**
     * Returns the number of tested methods.
     *
     * @return integer
     */
    abstract public function getNumTestedMethods();

    /**
     * Returns the number of functions.
     *
     * @return integer
     */
    abstract public function getNumFunctions();

    /**
     * Returns the number of tested functions.
     *
     * @return integer
     */
    abstract public function getNumTestedFunctions();
}
