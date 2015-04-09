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
 * Mocker for invocations which are sent from
 * PHPUnit_Framework_MockObject_MockObject objects.
 *
 * Keeps track of all expectations and stubs as well as registering
 * identifications for builders.
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Class available since Release 1.0.0
 */
class PHPUnit_Framework_MockObject_InvocationMocker implements PHPUnit_Framework_MockObject_Stub_MatcherCollection, PHPUnit_Framework_MockObject_Invokable, PHPUnit_Framework_MockObject_Builder_Namespace
{
    /**
     * @var PHPUnit_Framework_MockObject_Matcher_Invocation[]
     */
    protected $matchers = array();

    /**
     * @var PHPUnit_Framework_MockObject_Builder_Match[]
     */
    protected $builderMap = array();

    /**
     * @param PHPUnit_Framework_MockObject_Matcher_Invocation $matcher
     */
    public function addMatcher(PHPUnit_Framework_MockObject_Matcher_Invocation $matcher)
    {
        $this->matchers[] = $matcher;
    }

    /**
     * @since Method available since Release 1.1.0
     */
    public function hasMatchers()
    {
        foreach ($this->matchers as $matcher) {
            if ($matcher->hasMatchers()) {
                return TRUE;
            }
        }

        return FALSE;
    }

    /**
     * @param  mixed        $id
     * @return boolean|null
     */
    public function lookupId($id)
    {
        if (isset($this->builderMap[$id])) {
            return $this->builderMap[$id];
        }

        return NULL;
    }

    /**
     * @param  mixed                                      $id
     * @param  PHPUnit_Framework_MockObject_Builder_Match $builder
     * @throws PHPUnit_Framework_Exception
     */
    public function registerId($id, PHPUnit_Framework_MockObject_Builder_Match $builder)
    {
        if (isset($this->builderMap[$id])) {
            throw new PHPUnit_Framework_Exception(
              'Match builder with id <' . $id . '> is already registered.'
            );
        }

        $this->builderMap[$id] = $builder;
    }

    /**
     * @param  PHPUnit_Framework_MockObject_Matcher_Invocation       $matcher
     * @return PHPUnit_Framework_MockObject_Builder_InvocationMocker
     */
    public function expects(PHPUnit_Framework_MockObject_Matcher_Invocation $matcher)
    {
        return new PHPUnit_Framework_MockObject_Builder_InvocationMocker(
          $this, $matcher
        );
    }

    /**
     * @param  PHPUnit_Framework_MockObject_Invocation $invocation
     * @return mixed
     */
    public function invoke(PHPUnit_Framework_MockObject_Invocation $invocation)
    {
        $exception      = NULL;
        $hasReturnValue = FALSE;

        if (strtolower($invocation->methodName) == '__tostring') {
            $returnValue = '';
        } else {
            $returnValue = NULL;
        }

        foreach ($this->matchers as $match) {
            try {
                if ($match->matches($invocation)) {
                    $value = $match->invoked($invocation);

                    if (!$hasReturnValue) {
                        $returnValue    = $value;
                        $hasReturnValue = TRUE;
                    }
                }
            } catch (Exception $e) {
                $exception = $e;
            }
        }

        if ($exception !== NULL) {
            throw $exception;
        }

        return $returnValue;
    }

    /**
     * @param  PHPUnit_Framework_MockObject_Invocation $invocation
     * @return boolean
     */
    public function matches(PHPUnit_Framework_MockObject_Invocation $invocation)
    {
        foreach ($this->matchers as $matcher) {
            if (!$matcher->matches($invocation)) {
                return FALSE;
            }
        }

        return TRUE;
    }

    /**
     * @return boolean
     */
    public function verify()
    {
        foreach ($this->matchers as $matcher) {
            $matcher->verify();
        }
    }
}
