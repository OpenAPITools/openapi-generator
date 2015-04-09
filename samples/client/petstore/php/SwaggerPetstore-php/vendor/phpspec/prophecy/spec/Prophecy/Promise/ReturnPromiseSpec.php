<?php

namespace spec\Prophecy\Promise;

use PhpSpec\ObjectBehavior;

class ReturnPromiseSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith(array(42));
    }

    function it_is_promise()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Promise\PromiseInterface');
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_returns_value_it_was_constructed_with($object, $method)
    {
        $this->execute(array(), $object, $method)->shouldReturn(42);
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_always_returns_last_value_left_in_the_return_values($object, $method)
    {
        $this->execute(array(), $object, $method)->shouldReturn(42);
        $this->execute(array(), $object, $method)->shouldReturn(42);
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_consequently_returns_multiple_values_it_was_constructed_with($object, $method)
    {
        $this->beConstructedWith(array(42, 24, 12));

        $this->execute(array(), $object, $method)->shouldReturn(42);
        $this->execute(array(), $object, $method)->shouldReturn(24);
        $this->execute(array(), $object, $method)->shouldReturn(12);
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     */
    function it_returns_null_if_constructed_with_empty_array($object, $method)
    {
        $this->beConstructedWith(array());

        $this->execute(array(), $object, $method)->shouldReturn(null);
    }
}
