<?php

namespace spec\Prophecy\Prediction;

use PhpSpec\ObjectBehavior;

use RuntimeException;

class CallbackPredictionSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith('get_class');
    }

    function it_is_prediction()
    {
        $this->shouldHaveType('Prophecy\Prediction\PredictionInterface');
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     * @param \Prophecy\Prophecy\MethodProphecy $method
     * @param \Prophecy\Call\Call               $call
     */
    function it_proxies_call_to_callback($object, $method, $call)
    {
        $returnFirstCallCallback = function ($calls, $object, $method) {
            throw new RuntimeException;
        };

        $this->beConstructedWith($returnFirstCallCallback);

        $this->shouldThrow('RuntimeException')->duringCheck(array($call), $object, $method);
    }
}
