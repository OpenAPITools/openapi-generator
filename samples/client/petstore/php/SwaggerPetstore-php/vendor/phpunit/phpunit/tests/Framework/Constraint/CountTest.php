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
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @author     Jeroen Versteeg <jversteeg@gmail.com>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.7.30
 * @covers     PHPUnit_Framework_Constraint_Count
 */
class CountTest extends PHPUnit_Framework_TestCase
{
    public function testCount()
    {
        $countConstraint = new PHPUnit_Framework_Constraint_Count(3);
        $this->assertTrue($countConstraint->evaluate(array(1, 2, 3), '', true));

        $countConstraint = new PHPUnit_Framework_Constraint_Count(0);
        $this->assertTrue($countConstraint->evaluate(array(), '', true));

        $countConstraint = new PHPUnit_Framework_Constraint_Count(2);
        $it = new TestIterator(array(1, 2));
        $this->assertTrue($countConstraint->evaluate($it, '', true));
    }

    public function testCountDoesNotChangeIteratorKey()
    {
        $countConstraint = new PHPUnit_Framework_Constraint_Count(2);

        // test with 1st implementation of Iterator
        $it = new TestIterator(array(1, 2));

        $countConstraint->evaluate($it, '', true);
        $this->assertEquals(1, $it->current());

        $it->next();
        $countConstraint->evaluate($it, '', true);
        $this->assertEquals(2, $it->current());

        $it->next();
        $countConstraint->evaluate($it, '', true);
        $this->assertFalse($it->valid());

        // test with 2nd implementation of Iterator
        $it = new TestIterator2(array(1, 2));

        $countConstraint = new PHPUnit_Framework_Constraint_Count(2);
        $countConstraint->evaluate($it, '', true);
        $this->assertEquals(1, $it->current());

        $it->next();
        $countConstraint->evaluate($it, '', true);
        $this->assertEquals(2, $it->current());

        $it->next();
        $countConstraint->evaluate($it, '', true);
        $this->assertFalse($it->valid());
    }
}
