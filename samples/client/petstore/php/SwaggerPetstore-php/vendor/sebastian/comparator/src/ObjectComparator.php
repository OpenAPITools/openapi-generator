<?php
/*
 * This file is part of the Comparator package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace SebastianBergmann\Comparator;

/**
 * Compares objects for equality.
 *
 * @package    Comparator
 * @author     Bernhard Schussek <bschussek@2bepublished.at>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.github.com/sebastianbergmann/comparator
 */
class ObjectComparator extends ArrayComparator
{
    /**
     * Returns whether the comparator can compare two values.
     *
     * @param  mixed $expected The first value to compare
     * @param  mixed $actual The second value to compare
     * @return boolean
     */
    public function accepts($expected, $actual)
    {
        return is_object($expected) && is_object($actual);
    }

    /**
     * Asserts that two values are equal.
     *
     * @param  mixed $expected The first value to compare
     * @param  mixed $actual The second value to compare
     * @param  float $delta The allowed numerical distance between two values to
     *                      consider them equal
     * @param  bool  $canonicalize If set to TRUE, arrays are sorted before
     *                             comparison
     * @param  bool  $ignoreCase If set to TRUE, upper- and lowercasing is
     *                           ignored when comparing string values
     * @param  array $processed
     * @throws ComparisonFailure Thrown when the comparison
     *                           fails. Contains information about the
     *                           specific errors that lead to the failure.
     */
    public function assertEquals($expected, $actual, $delta = 0.0, $canonicalize = false, $ignoreCase = false, array &$processed = array())
    {
        if (get_class($actual) !== get_class($expected)) {
            throw new ComparisonFailure(
                $expected,
                $actual,
                $this->exporter->export($expected),
                $this->exporter->export($actual),
                false,
                sprintf(
                    '%s is not instance of expected class "%s".',
                    $this->exporter->export($actual),
                    get_class($expected)
                )
            );
        }

        // don't compare twice to allow for cyclic dependencies
        if (in_array(array($actual, $expected), $processed, true) ||
            in_array(array($expected, $actual), $processed, true)) {
            return;
        }

        $processed[] = array($actual, $expected);

        // don't compare objects if they are identical
        // this helps to avoid the error "maximum function nesting level reached"
        // CAUTION: this conditional clause is not tested
        if ($actual !== $expected) {
            try {
                parent::assertEquals(
                    $this->toArray($expected),
                    $this->toArray($actual),
                    $delta,
                    $canonicalize,
                    $ignoreCase,
                    $processed
                );
            } catch (ComparisonFailure $e) {
                throw new ComparisonFailure(
                    $expected,
                    $actual,
                    // replace "Array" with "MyClass object"
                    substr_replace($e->getExpectedAsString(), get_class($expected) . ' Object', 0, 5),
                    substr_replace($e->getActualAsString(), get_class($actual) . ' Object', 0, 5),
                    false,
                    'Failed asserting that two objects are equal.'
                );
            }
        }
    }

    /**
     * Converts an object to an array containing all of its private, protected
     * and public properties.
     *
     * @param  object $object
     * @return array
     */
    protected function toArray($object)
    {
        return $this->exporter->toArray($object);
    }
}
