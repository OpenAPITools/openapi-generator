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
 * Main matcher which defines a full expectation using method, parameter and
 * invocation matchers.
 * This matcher encapsulates all the other matchers and allows the builder to
 * set the specific matchers when the appropriate methods are called (once(),
 * where() etc.).
 *
 * All properties are public so that they can easily be accessed by the builder.
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Class available since Release 1.0.0
 */
class PHPUnit_Framework_MockObject_Matcher implements PHPUnit_Framework_MockObject_Matcher_Invocation
{
    /**
     * @var PHPUnit_Framework_MockObject_Matcher_Invocation
     */
    public $invocationMatcher;

    /**
     * @var mixed
     */
    public $afterMatchBuilderId = NULL;

    /**
     * @var boolean
     */
    public $afterMatchBuilderIsInvoked = FALSE;

    /**
     * @var PHPUnit_Framework_MockObject_Matcher_MethodName
     */
    public $methodNameMatcher = NULL;

    /**
     * @var PHPUnit_Framework_MockObject_Matcher_Parameters
     */
    public $parametersMatcher = NULL;

    /**
     * @var PHPUnit_Framework_MockObject_Stub
     */
    public $stub = NULL;

    /**
     * @param PHPUnit_Framework_MockObject_Matcher_Invocation $invocationMatcher
     */
    public function __construct(PHPUnit_Framework_MockObject_Matcher_Invocation $invocationMatcher)
    {
        $this->invocationMatcher = $invocationMatcher;
    }

    /**
     * @return string
     */
    public function toString()
    {
        $list = array();

        if ($this->invocationMatcher !== NULL) {
            $list[] = $this->invocationMatcher->toString();
        }

        if ($this->methodNameMatcher !== NULL) {
            $list[] = 'where ' . $this->methodNameMatcher->toString();
        }

        if ($this->parametersMatcher !== NULL) {
            $list[] = 'and ' . $this->parametersMatcher->toString();
        }

        if ($this->afterMatchBuilderId !== NULL) {
            $list[] = 'after ' . $this->afterMatchBuilderId;
        }

        if ($this->stub !== NULL) {
            $list[] = 'will ' . $this->stub->toString();
        }

        return join(' ', $list);
    }

    /**
     * @param  PHPUnit_Framework_MockObject_Invocation $invocation
     * @return mixed
     */
    public function invoked(PHPUnit_Framework_MockObject_Invocation $invocation)
    {
        if ($this->invocationMatcher === NULL) {
            throw new PHPUnit_Framework_Exception(
              'No invocation matcher is set'
            );
        }

        if ($this->methodNameMatcher === NULL) {
            throw new PHPUnit_Framework_Exception('No method matcher is set');
        }

        if ($this->afterMatchBuilderId !== NULL) {
            $builder = $invocation->object
                                  ->__phpunit_getInvocationMocker()
                                  ->lookupId($this->afterMatchBuilderId);

            if (!$builder) {
                throw new PHPUnit_Framework_Exception(
                  sprintf(
                    'No builder found for match builder identification <%s>',

                    $this->afterMatchBuilderId
                  )
                );
            }

            $matcher = $builder->getMatcher();

            if ($matcher && $matcher->invocationMatcher->hasBeenInvoked()) {
                $this->afterMatchBuilderIsInvoked = TRUE;
            }
        }

        $this->invocationMatcher->invoked($invocation);

        try {
            if ( $this->parametersMatcher !== NULL &&
                !$this->parametersMatcher->matches($invocation)) {
                $this->parametersMatcher->verify();
            }
        } catch (PHPUnit_Framework_ExpectationFailedException $e) {
            throw new PHPUnit_Framework_ExpectationFailedException(
              sprintf(
                "Expectation failed for %s when %s\n%s",

                $this->methodNameMatcher->toString(),
                $this->invocationMatcher->toString(),
                $e->getMessage()
              ),
              $e->getComparisonFailure()
            );
        }

        if ($this->stub) {
            return $this->stub->invoke($invocation);
        }

        return NULL;
    }

    /**
     * @param  PHPUnit_Framework_MockObject_Invocation $invocation
     * @return boolean
     */
    public function matches(PHPUnit_Framework_MockObject_Invocation $invocation)
    {
        if ($this->afterMatchBuilderId !== NULL) {
            $builder = $invocation->object
                                  ->__phpunit_getInvocationMocker()
                                  ->lookupId($this->afterMatchBuilderId);

            if (!$builder) {
                throw new PHPUnit_Framework_Exception(
                  sprintf(
                    'No builder found for match builder identification <%s>',

                    $this->afterMatchBuilderId
                  )
                );
            }

            $matcher = $builder->getMatcher();

            if (!$matcher) {
                return FALSE;
            }

            if (!$matcher->invocationMatcher->hasBeenInvoked()) {
                return FALSE;
            }
        }

        if ($this->invocationMatcher === NULL) {
            throw new PHPUnit_Framework_Exception(
              'No invocation matcher is set'
            );
        }

        if ($this->methodNameMatcher === NULL) {
            throw new PHPUnit_Framework_Exception('No method matcher is set');
        }

        if (!$this->invocationMatcher->matches($invocation)) {
            return FALSE;
        }

        try {
            if (!$this->methodNameMatcher->matches($invocation)) {
                return FALSE;
            }
        } catch (PHPUnit_Framework_ExpectationFailedException $e) {
            throw new PHPUnit_Framework_ExpectationFailedException(
              sprintf(
                "Expectation failed for %s when %s\n%s",

                $this->methodNameMatcher->toString(),
                $this->invocationMatcher->toString(),
                $e->getMessage()
              ),
              $e->getComparisonFailure()
            );
        }

        return TRUE;
    }

    /**
     * @throws PHPUnit_Framework_Exception
     * @throws PHPUnit_Framework_ExpectationFailedException
     */
    public function verify()
    {
        if ($this->invocationMatcher === NULL) {
            throw new PHPUnit_Framework_Exception(
              'No invocation matcher is set'
            );
        }

        if ($this->methodNameMatcher === NULL) {
            throw new PHPUnit_Framework_Exception('No method matcher is set');
        }

        try {
            $this->invocationMatcher->verify();

            if ($this->parametersMatcher === NULL) {
                $this->parametersMatcher = new PHPUnit_Framework_MockObject_Matcher_AnyParameters;
            }

            $invocationIsAny = get_class($this->invocationMatcher) === 'PHPUnit_Framework_MockObject_Matcher_AnyInvokedCount';
            $invocationIsNever = get_class($this->invocationMatcher) === 'PHPUnit_Framework_MockObject_Matcher_InvokedCount' && $this->invocationMatcher->isNever();
            if (!$invocationIsAny && !$invocationIsNever) {
                $this->parametersMatcher->verify();
            }
        } catch (PHPUnit_Framework_ExpectationFailedException $e) {
            throw new PHPUnit_Framework_ExpectationFailedException(
              sprintf(
                "Expectation failed for %s when %s.\n%s",

                $this->methodNameMatcher->toString(),
                $this->invocationMatcher->toString(),
                PHPUnit_Framework_TestFailure::exceptionToString($e)
              )
            );
        }
    }

    /**
     * @since Method available since Release 1.2.4
     */
    public function hasMatchers()
    {
        if ($this->invocationMatcher !== NULL &&
            !$this->invocationMatcher instanceof PHPUnit_Framework_MockObject_Matcher_AnyInvokedCount) {
            return TRUE;
        }

        return FALSE;
    }
}
