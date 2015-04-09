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
 *
 *
 * @package    PHPUnit
 * @subpackage Framework_Constraint
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @author     Bernhard Schussek <bschussek@2bepublished.at>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.6.0
 */
class PHPUnit_Framework_Constraint_Count extends PHPUnit_Framework_Constraint
{
    /**
     * @var integer
     */
    protected $expectedCount = 0;

    /**
     * @param integer $expected
     */
    public function __construct($expected)
    {
        parent::__construct();
        $this->expectedCount = $expected;
    }

    /**
     * Evaluates the constraint for parameter $other. Returns true if the
     * constraint is met, false otherwise.
     *
     * @param  mixed   $other
     * @return boolean
     */
    protected function matches($other)
    {
        return $this->expectedCount === $this->getCountOf($other);
    }

    /**
     * @param  mixed   $other
     * @return boolean
     */
    protected function getCountOf($other)
    {
        if ($other instanceof Countable || is_array($other)) {
            return count($other);
        } elseif ($other instanceof Traversable) {
            if ($other instanceof IteratorAggregate) {
                $iterator = $other->getIterator();
            } else {
                $iterator = $other;
            }

            $key = $iterator->key();
            $count = iterator_count($iterator);

            // manually rewind $iterator to previous key, since iterator_count
            // moves pointer
            if ($key !== null) {
                $iterator->rewind();
                while ($iterator->valid() && $key !== $iterator->key()) {
                    $iterator->next();
                }
            }

            return $count;
        }
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
            'actual size %d matches expected size %d',
            $this->getCountOf($other),
            $this->expectedCount
        );
    }

    /**
     * @return string
     */
    public function toString()
    {
        return sprintf(
            'count matches %d',
            $this->expectedCount
        );
    }
}
