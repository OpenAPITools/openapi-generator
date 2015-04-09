<?php

namespace spec\Prophecy\Promise;

use PhpSpec\ObjectBehavior;

class ThrowPromiseSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith('RuntimeException');
    }

    function it_is_promise()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Promise\PromiseInterface');
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_instantiates_and_throws_exception_from_provided_classname($object, $method)
    {
        $this->beConstructedWith('InvalidArgumentException');

        $this->shouldThrow('InvalidArgumentException')
            ->duringExecute(array(), $object, $method);
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_instantiates_exceptions_with_required_arguments($object, $method)
    {
        $this->beConstructedWith('spec\Prophecy\Promise\RequiredArgumentException');

        $this->shouldThrow('spec\Prophecy\Promise\RequiredArgumentException')
            ->duringExecute(array(), $object, $method);
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_throws_provided_exception($object, $method)
    {
        $this->beConstructedWith($exc = new \RuntimeException('Some exception'));

        $this->shouldThrow($exc)->duringExecute(array(), $object, $method);
    }
}

class RequiredArgumentException extends \Exception
{
    final public function __construct($message, $code) {}
}
