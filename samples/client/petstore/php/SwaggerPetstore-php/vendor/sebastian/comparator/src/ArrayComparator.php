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
 * Compares arrays for equality.
 *
 * @package    Comparator
 * @author     Bernhard Schussek <bschussek@2bepublished.at>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.github.com/sebastianbergmann/comparator
 */
class ArrayComparator extends Comparator
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
        return is_array($expected) && is_array($actual);
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
        if ($canonicalize) {
            sort($expected);
            sort($actual);
        }

        $remaining = $actual;
        $expString = $actString = "Array (\n";
        $equal     = true;

        foreach ($expected as $key => $value) {
            unset($remaining[$key]);

            if (!array_key_exists($key, $actual)) {
                $expString .= sprintf(
                    "    %s => %s\n",
                    $this->exporter->export($key),
                    $this->exporter->shortenedExport($value)
                );

                $equal = false;

                continue;
            }

            try {
                $comparator = $this->factory->getComparatorFor($value, $actual[$key]);
                $comparator->assertEquals($value, $actual[$key], $delta, $canonicalize, $ignoreCase, $processed);

                $expString .= sprintf(
                    "    %s => %s\n",
                    $this->exporter->export($key),
                    $this->exporter->shortenedExport($value)
                );
                $actString .= sprintf(
                    "    %s => %s\n",
                    $this->exporter->export($key),
                    $this->exporter->shortenedExport($actual[$key])
                );
            } catch (ComparisonFailure $e) {
                $expString .= sprintf(
                    "    %s => %s\n",
                    $this->exporter->export($key),
                    $e->getExpectedAsString()
                    ? $this->indent($e->getExpectedAsString())
                    : $this->exporter->shortenedExport($e->getExpected())
                );

                $actString .= sprintf(
                    "    %s => %s\n",
                    $this->exporter->export($key),
                    $e->getActualAsString()
                    ? $this->indent($e->getActualAsString())
                    : $this->exporter->shortenedExport($e->getActual())
                );

                $equal = false;
            }
        }

        foreach ($remaining as $key => $value) {
            $actString .= sprintf(
                "    %s => %s\n",
                $this->exporter->export($key),
                $this->exporter->shortenedExport($value)
            );

            $equal = false;
        }

        $expString .= ')';
        $actString .= ')';

        if (!$equal) {
            throw new ComparisonFailure(
                $expected,
                $actual,
                $expString,
                $actString,
                false,
                'Failed asserting that two arrays are equal.'
            );
        }
    }

    protected function indent($lines)
    {
        return trim(str_replace("\n", "\n    ", $lines));
    }
}
