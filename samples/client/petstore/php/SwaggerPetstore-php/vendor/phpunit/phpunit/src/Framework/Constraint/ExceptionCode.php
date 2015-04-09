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
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.6.6
 */
class PHPUnit_Framework_Constraint_ExceptionCode extends PHPUnit_Framework_Constraint
{
    /**
     * @var integer
     */
    protected $expectedCode;

    /**
     * @param integer $expected
     */
    public function __construct($expected)
    {
        parent::__construct();
        $this->expectedCode = $expected;
    }

    /**
     * Evaluates the constraint for parameter $other. Returns true if the
     * constraint is met, false otherwise.
     *
     * @param  Exception $other
     * @return boolean
     */
    protected function matches($other)
    {
        return (string) $other->getCode() == (string) $this->expectedCode;
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
            '%s is equal to expected exception code %s',
            $this->exporter->export($other->getCode()),
            $this->exporter->export($this->expectedCode)
        );
    }

    /**
     * @return string
     */
    public function toString()
    {
        return 'exception code is ';
    }
}
