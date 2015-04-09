<?php

namespace spec\Prophecy\Exception\Call;

use PhpSpec\ObjectBehavior;
use spec\Prophecy\Exception\Prophecy\Prophecy;

class UnexpectedCallExceptionSpec extends ObjectBehavior
{
    /**
     * @param \Prophecy\Prophecy\ObjectProphecy $objectProphecy
     */
    function let($objectProphecy)
    {
        $this->beConstructedWith('msg', $objectProphecy, 'getName', array('arg1', 'arg2'));
    }

    function it_is_prophecy_exception()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Exception\Prophecy\ObjectProphecyException');
    }

    function it_exposes_method_name_through_getter()
    {
        $this->getMethodName()->shouldReturn('getName');
    }

    function it_exposes_arguments_through_getter()
    {
        $this->getArguments()->shouldReturn(array('arg1', 'arg2'));
    }
}
