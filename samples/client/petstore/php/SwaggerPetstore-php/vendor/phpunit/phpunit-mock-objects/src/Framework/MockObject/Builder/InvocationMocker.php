<?php
/*
 * This file is part of the PHPUnit_MockObject package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Builder for mocked or stubbed invocations.
 *
 * Provides methods for building expectations without having to resort to
 * instantiating the various matchers manually. These methods also form a
 * more natural way of reading the expectation. This class should be together
 * with the test case PHPUnit_Framework_MockObject_TestCase.
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Class available since Release 1.0.0
 */
class PHPUnit_Framework_MockObject_Builder_InvocationMocker implements PHPUnit_Framework_MockObject_Builder_MethodNameMatch
{
    /**
     * @var PHPUnit_Framework_MockObject_Stub_MatcherCollection
     */
    protected $collection;

    /**
     * @var PHPUnit_Framework_MockObject_Matcher
     */
    protected $matcher;

    /**
     * @param PHPUnit_Framework_MockObject_Stub_MatcherCollection $collection
     * @param PHPUnit_Framework_MockObject_Matcher_Invocation     $invocationMatcher
     */
    public function __construct(PHPUnit_Framework_MockObject_Stub_MatcherCollection $collection, PHPUnit_Framework_MockObject_Matcher_Invocation $invocationMatcher)
    {
        $this->collection = $collection;
        $this->matcher    = new PHPUnit_Framework_MockObject_Matcher(
          $invocationMatcher
        );

        $this->collection->addMatcher($this->matcher);
    }

    /**
     * @return PHPUnit_Framework_MockObject_Matcher
     */
    public function getMatcher()
    {
        return $this->matcher;
    }

    /**
     * @param  mixed                                                 $id
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function id($id)
    {
        $this->collection->registerId($id, $this);

        return $this;
    }

    /**
     * @param  PHPUnit_Framework_MockObject_Stub                     $stub
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function will(PHPUnit_Framework_MockObject_Stub $stub)
    {
        $this->matcher->stub = $stub;

        return $this;
    }

    /**
     * @param  mixed                                                 $value
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function willReturn($value)
    {
        $stub = new PHPUnit_Framework_MockObject_Stub_Return(
          $value
        );

        return $this->will($stub);
    }

    /**
     * @param  array                                                 $valueMap
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function willReturnMap(array $valueMap)
    {
        $stub = new PHPUnit_Framework_MockObject_Stub_ReturnValueMap(
          $valueMap
        );

        return $this->will($stub);
    }

    /**
     * @param  mixed                                                 $argumentIndex
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function willReturnArgument($argumentIndex)
    {
        $stub = new PHPUnit_Framework_MockObject_Stub_ReturnArgument(
          $argumentIndex
        );

        return $this->will($stub);
    }

    /**
     * @param  callable                                              $callback
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function willReturnCallback($callback)
    {
        $stub = new PHPUnit_Framework_MockObject_Stub_ReturnCallback(
          $callback
        );

        return $this->will($stub);
    }

    /**
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function willReturnSelf()
    {
        $stub = new PHPUnit_Framework_MockObject_Stub_ReturnSelf();

        return $this->will($stub);
    }

    /**
     * @param  mixed                                                 $value, ...
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function willReturnOnConsecutiveCalls()
    {
        $args = func_get_args();

        $stub = new PHPUnit_Framework_MockObject_Stub_ConsecutiveCalls($args);

        return $this->will($stub);
    }

    /**
     * @param  Exception                                             $exception
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function willThrowException(Exception $exception)
    {
        $stub = new PHPUnit_Framework_MockObject_Stub_Exception($exception);

        return $this->will($stub);
    }

    /**
     * @param  mixed                                                 $id
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function after($id)
    {
        $this->matcher->afterMatchBuilderId = $id;

        return $this;
    }

    /**
     * Validate that a parameters matcher can be defined, throw exceptions otherwise.
     *
     * @throws PHPUnit_Framework_Exception
     */
    private function canDefineParameters()
    {
        if ($this->matcher->methodNameMatcher === NULL) {
            throw new PHPUnit_Framework_Exception(
              'Method name matcher is not defined, cannot define parameter ' .
              ' matcher without one'
            );
        }

        if ($this->matcher->parametersMatcher !== NULL) {
            throw new PHPUnit_Framework_Exception(
              'Parameter matcher is already defined, cannot redefine'
            );
        }
    }

    /**
     * @param  mixed                                                 $argument, ...
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function with()
    {
        $args = func_get_args();

        $this->canDefineParameters();

        $this->matcher->parametersMatcher = new PHPUnit_Framework_MockObject_Matcher_Parameters($args);

        return $this;
    }

    /**
     * @param  mixed ...$argument
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function withConsecutive() {

        $args = func_get_args();

        $this->canDefineParameters();

        $this->matcher->parametersMatcher =
          new PHPUnit_Framework_MockObject_Matcher_ConsecutiveParameters($args);

        return $this;
    }

    /**
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function withAnyParameters()
    {
        $this->canDefineParameters();

        $this->matcher->parametersMatcher = new PHPUnit_Framework_MockObject_Matcher_AnyParameters;

        return $this;
    }

    /**
     * @param  PHPUnit_Framework_Constraint|string                   $constraint
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function method($constraint)
    {
        if ($this->matcher->methodNameMatcher !== NULL) {
            throw new PHPUnit_Framework_Exception(
              'Method name matcher is already defined, cannot redefine'
            );
        }

        $this->matcher->methodNameMatcher = new PHPUnit_Framework_MockObject_Matcher_MethodName($constraint);

        return $this;
    }
}
