<?php

namespace spec\Prophecy\Call;

use PhpSpec\ObjectBehavior;

class CallSpec extends ObjectBehavior
{
    /**
     * @param \Exception $exception
     */
    function let($exception)
    {
        $this->beConstructedWith('setValues', array(5, 2), 42, $exception, 'some_file.php', 23);
    }

    function it_exposes_method_name_through_getter()
    {
        $this->getMethodName()->shouldReturn('setValues');
    }

    function it_exposes_arguments_through_getter()
    {
        $this->getArguments()->shouldReturn(array(5, 2));
    }

    function it_exposes_return_value_through_getter()
    {
        $this->getReturnValue()->shouldReturn(42);
    }

    function it_exposes_exception_through_getter($exception)
    {
        $this->getException()->shouldReturn($exception);
    }

    function it_exposes_file_and_line_through_getter()
    {
        $this->getFile()->shouldReturn('some_file.php');
        $this->getLine()->shouldReturn(23);
    }

    function it_returns_shortpath_to_callPlace()
    {
        $this->getCallPlace()->shouldReturn('some_file.php:23');
    }

    function it_returns_unknown_as_callPlace_if_no_file_or_line_provided()
    {
        $this->beConstructedWith('setValues', array(), 0, null, null, null);

        $this->getCallPlace()->shouldReturn('unknown');
    }
}
