<?php

namespace spec\Prophecy\Prediction;

use PhpSpec\ObjectBehavior;

class CallTimesPredictionSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith(2);
    }

    function it_is_prediction()
    {
        $this->shouldHaveType('Prophecy\Prediction\PredictionInterface');
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     * @param \Prophecy\Call\Call               $call1
     * @param \Prophecy\Call\Call               $call2
     */
    function it_does_nothing_if_there_were_exact_amount_of_calls_being_made(
        $object, $method, $call1, $call2
    )
    {
        $this->check(array($call1, $call2), $object, $method)->shouldReturn(null);
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy    $object
     * @param \Prophecy\Prophecy\MethodProphecy    $method
     * @param \Prophecy\Call\Call                  $call
     * @param \Prophecy\Argument\ArgumentsWildcard $arguments
     */
    function it_throws_UnexpectedCallsCountException_if_calls_found(
        $object, $method, $call, $arguments
    )
    {
        $method->getObjectProphecy()->willReturn($object);
        $method->getMethodName()->willReturn('getName');
        $method->getArgumentsWildcard()->willReturn($arguments);
        $arguments->__toString()->willReturn('123');

        $call->getMethodName()->willReturn('getName');
        $call->getArguments()->willReturn(array(5, 4, 'three'));
        $call->getCallPlace()->willReturn('unknown');

        $this->shouldThrow('Prophecy\Exception\Prediction\UnexpectedCallsCountException')
            ->duringCheck(array($call), $object, $method);
    }
}
