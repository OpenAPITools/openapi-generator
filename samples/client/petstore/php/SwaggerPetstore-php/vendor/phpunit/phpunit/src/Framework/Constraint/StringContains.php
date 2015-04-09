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
 * Constraint that asserts that the string it is evaluated for contains
 * a given string.
 *
 * Uses strpos() to find the position of the string in the input, if not found
 * the evaluation fails.
 *
 * The sub-string is passed in the constructor.
 *
 * @package    PHPUnit
 * @subpackage Framework_Constraint
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @author     Bernhard Schussek <bschussek@2bepublished.at>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.0.0
 */
class PHPUnit_Framework_Constraint_StringContains extends PHPUnit_Framework_Constraint
{
    /**
     * @var string
     */
    protected $string;

    /**
     * @var boolean
     */
    protected $ignoreCase;

    /**
     * @param string  $string
     * @param boolean $ignoreCase
     */
    public function __construct($string, $ignoreCase = false)
    {
        parent::__construct();

        $this->string     = $string;
        $this->ignoreCase = $ignoreCase;
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
        if ($this->ignoreCase) {
            return stripos($other, $this->string) !== false;
        } else {
            return strpos($other, $this->string) !== false;
        }
    }

    /**
     * Returns a string representation of the constraint.
     *
     * @return string
     */
    public function toString()
    {
        if ($this->ignoreCase) {
            $string = strtolower($this->string);
        } else {
            $string = $this->string;
        }

        return sprintf(
            'contains "%s"',
            $string
        );
    }
}
