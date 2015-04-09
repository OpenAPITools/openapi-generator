<?php
/*
 * This file is part of PHPUnit.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Constraint that asserts that the class it is evaluated for has a given
 * attribute.
 *
 * The attribute name is passed in the constructor.
 *
 * @package    PHPUnit
 * @subpackage Framework_Constraint
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @author     Bernhard Schussek <bschussek@2bepublished.at>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.1.0
 */
class PHPUnit_Framework_Constraint_ClassHasAttribute extends PHPUnit_Framework_Constraint
{
    /**
     * @var string
     */
    protected $attributeName;

    /**
     * @param string $attributeName
     */
    public function __construct($attributeName)
    {
        parent::__construct();
        $this->attributeName = $attributeName;
    }

    /**
     * Evaluates the constraint for parameter $other. Returns true if the
     * constraint is met, false otherwise.
     *
     * @param  mixed $other Value or object to evaluate.
     * @return bool
     */
    protected function matches($other)
    {
        $class = new ReflectionClass($other);

        return $class->hasProperty($this->attributeName);
    }

    /**
     * Returns a string representation of the constraint.
     *
     * @return string
     */
    public function toString()
    {
        return sprintf(
            'has attribute "%s"',
            $this->attributeName
        );
    }

    /**
     * Returns the description of the failure
     *
     * The beginning of failure messages is "Failed asserting that" in most
     * cases. This method should return the second part of that sentence.
     *
     * @param  mixed  $other Evaluated value or object.
     * @return string
     */
    protected function failureDescription($other)
    {
        return sprintf(
            '%sclass "%s" %s',
            is_object($other) ? 'object of ' : '',
            is_object($other) ? get_class($other) : $other,
            $this->toString()
        );
    }
}
