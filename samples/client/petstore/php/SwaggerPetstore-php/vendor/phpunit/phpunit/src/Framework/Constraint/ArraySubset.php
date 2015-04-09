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
 * Constraint that asserts that the array it is evaluated for has a specified subset.
 *
 * Uses array_replace_recursive() to check if a key value subset is part of the
 * subject array.
 *
 * @package    PHPUnit
 * @subpackage Framework_Constraint
 * @author     Márcio Almada <marcio3w@gmail.com>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 4.4.0
 */
class PHPUnit_Framework_Constraint_ArraySubset extends PHPUnit_Framework_Constraint
{
    /**
     * @var array|ArrayAccess
     */
    protected $subset;

    /**
     * @var boolean
     */
    protected $strict;

    /**
     * @param array|ArrayAccess $subset
     * @param boolean           $strict Check for object identity
     */
    public function __construct($subset, $strict = false)
    {
        parent::__construct();
        $this->strict  = $strict;
        $this->subset = $subset;
    }

    /**
     * Evaluates the constraint for parameter $other. Returns true if the
     * constraint is met, false otherwise.
     *
     * @param  array|ArrayAccess $other  Array or ArrayAcess object to evaluate.
     * @return bool
     */
    protected function matches($other)
    {
        $patched = array_replace_recursive($other, $this->subset);

        if ($this->strict) {
            return $other === $patched;
        } else {
            return $other == $patched;
        }
    }

    /**
     * Returns a string representation of the constraint.
     *
     * @return string
     */
    public function toString()
    {
        return 'has the subset ' . $this->exporter->export($this->subset);
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
        return 'an array ' . $this->toString();
    }
}
