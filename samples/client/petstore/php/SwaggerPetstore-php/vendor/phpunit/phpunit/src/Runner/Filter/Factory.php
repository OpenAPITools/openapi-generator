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
 * @package    PHPUnit
 * @subpackage Runner
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 4.0.0
 */
class PHPUnit_Runner_Filter_Factory
{
    /**
     * @var array
     */
    private $filters = array();

    /**
     * @param ReflectionClass $filter
     * @param mixed           $args
     */
    public function addFilter(ReflectionClass $filter, $args)
    {
        if (!$filter->isSubclassOf('RecursiveFilterIterator')) {
            throw new InvalidArgumentException(
                sprintf(
                    'Class "%s" does not extend RecursiveFilterIterator',
                    $filter->name
                )
            );
        }

        $this->filters[] = array($filter, $args);
    }

    /**
     * @return FilterIterator
     */
    public function factory(Iterator $iterator, PHPUnit_Framework_TestSuite $suite)
    {
        foreach ($this->filters as $filter) {
            list($class, $args) = $filter;
            $iterator = $class->newInstance($iterator, $args, $suite);
        }

        return $iterator;
    }
}
