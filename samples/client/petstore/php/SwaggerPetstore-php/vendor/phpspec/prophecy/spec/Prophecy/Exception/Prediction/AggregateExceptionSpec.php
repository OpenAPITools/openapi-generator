<?php

namespace spec\Prophecy\Exception\Prediction;

use PhpSpec\ObjectBehavior;

class AggregateExceptionSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith(null);
    }

    function it_is_prediction_exception()
    {
        $this->shouldBeAnInstanceOf('RuntimeException');
        $this->shouldBeAnInstanceOf('Prophecy\Exception\Prediction\PredictionException');
    }

    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $object
     */
    function it_can_store_objectProphecy_link($object)
    {
        $this->setObjectProphecy($object);
        $this->getObjectProphecy()->shouldReturn($object);
    }

    function it_should_not_have_exceptions_at_the_beginning()
    {
        $this->getExceptions()->shouldHaveCount(0);
    }

    /**
     * @param \Prophecy\Exception\Prediction\PredictionException $exception
     */
    function it_should_append_exception_through_append_method($exception)
    {
        $exception->getMessage()->willReturn('Exception #1');

        $this->append($exception);

        $this->getExceptions()->shouldReturn(array($exception));
    }

    /**
     * @param \Prophecy\Exception\Prediction\PredictionException $exception
     */
    function it_should_update_message_during_append($exception)
    {
        $exception->getMessage()->willReturn('Exception #1');

        $this->append($exception);

        $this->getMessage()->shouldReturn("  Exception #1");
    }
}
