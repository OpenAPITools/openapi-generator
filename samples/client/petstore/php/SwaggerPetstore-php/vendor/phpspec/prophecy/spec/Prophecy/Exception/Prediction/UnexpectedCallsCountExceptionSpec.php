<?php

namespace spec\Prophecy\Exception\Prediction;

use PhpSpec\ObjectBehavior;

class UnexpectedCallsCountExceptionSpec extends ObjectBehavior
{
    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $objectProphecy
     * @param \Prophecy\Prophecy\MethodProphecy $methodProphecy
     * @param \Prophecy\Call\Call               $call1
     * @param \Prophecy\Call\Call               $call2
     */
    function let($objectProphecy, $methodProphecy, $call1, $call2)
    {
        $methodProphecy->getObjectProphecy()->willReturn($objectProphecy);

        $this->beConstructedWith('message', $methodProphecy, 5, array($call1, $call2));
    }

    function it_extends_UnexpectedCallsException()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Exception\Prediction\UnexpectedCallsException');
    }

    function it_should_expose_expectedCount_through_getter()
    {
        $this->getExpectedCount()->shouldReturn(5);
    }
}
