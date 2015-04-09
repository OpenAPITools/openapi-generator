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
 * Constraint that evaluates against a specified closure.
 *
 * @package    PHPUnit
 * @subpackage Framework_Constraint
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @author     Timon Rapp <timon@zaeda.net>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 */
class PHPUnit_Framework_Constraint_Callback extends PHPUnit_Framework_Constraint
{
    private $callback;

    /**
     * @param  callable                    $callback
     * @throws PHPUnit_Framework_Exception
     */
    public function __construct($callback)
    {
        if (!is_callable($callback)) {
            throw PHPUnit_Util_InvalidArgumentHelper::factory(
                1,
                'callable'
            );
        }

        parent::__construct();

        $this->callback = $callback;
    }

    /**
     * Evaluates the constraint for parameter $value. Returns true if the
     * constraint is met, false otherwise.
     *
     * @param  mixed $other Value or object to evaluate.
     * @return bool
     */
    protected function matches($other)
    {
        return call_user_func($this->callback, $other);
    }

    /**
     * Returns a string representation of the constraint.
     *
     * @return string
     */
    public function toString()
    {
        return 'is accepted by specified callback';
    }
}
