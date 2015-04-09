<?php

namespace spec\Prophecy\Promise;

use PhpSpec\ObjectBehavior;

class ReturnArgumentPromiseSpec extends ObjectBehavior
{
    function it_is_promise()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Promise\PromiseInterface');
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_should_return_first_argument_if_provided($object, $method)
    {
        $this->execute(array('one', 'two'), $object, $method)->shouldReturn('one');
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_should_return_null_if_no_arguments_provided($object, $method)
    {
        $this->execute(array(), $object, $method)->shouldReturn(null);
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_should_return_nth_argument_if_provided($object, $method)
    {
        $this->beConstructedWith(1);
        $this->execute(array('one', 'two'), $object, $method)->shouldReturn('two');
    }
}
