<?php

namespace spec\Prophecy\Argument\Token;

use PhpSpec\ObjectBehavior;

class CallbackTokenSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith('get_class');
    }

    function it_implements_TokenInterface()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Argument\Token\TokenInterface');
    }

    function it_is_not_last()
    {
        $this->shouldNotBeLast();
    }

    function it_scores_7_if_argument_matches_callback()
    {
        $this->beConstructedWith(function ($argument) { return 2 === $argument; });

        $this->scoreArgument(2)->shouldReturn(7);
    }

    function it_does_not_scores_if_argument_does_not_match_callback()
    {
        $this->beConstructedWith(function ($argument) { return 2 === $argument; });

        $this->scoreArgument(5)->shouldReturn(false);
    }

    function its_string_representation_should_tell_that_its_callback()
    {
        $this->__toString()->shouldReturn('callback()');
    }
}
