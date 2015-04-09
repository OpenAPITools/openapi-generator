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

use SebastianBergmann\Exporter\Exporter;

/**
 * Abstract base class for comparators which compare values for equality.
 *
 * @package    Comparator
 * @subpackage Framework
 * @author     Bernhard Schussek <bschussek@2bepublished.at>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.github.com/sebastianbergmann/comparator
 */
abstract class Comparator
{
    /**
     * @var Factory
     */
    protected $factory;

    /**
     * @var Exporter
     */
    protected $exporter;

    public function __construct()
    {
        $this->exporter = new Exporter;
    }

    /**
     * @param Factory $factory
     */
    public function setFactory(Factory $factory)
    {
        $this->factory = $factory;
    }

    /**
     * Returns whether the comparator can compare two values.
     *
     * @param  mixed $expected The first value to compare
     * @param  mixed $actual The second value to compare
     * @return boolean
     */
    abstract public function accepts($expected, $actual);

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
     * @throws ComparisonFailure Thrown when the comparison
     *                           fails. Contains information about the
     *                           specific errors that lead to the failure.
     */
    abstract public function assertEquals($expected, $actual, $delta = 0.0, $canonicalize = false, $ignoreCase = false);
}
