<?php

namespace spec\Prophecy\Argument\Token;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class StringContainsTokenSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith('a substring');
    }

    function it_is_initializable()
    {
        $this->shouldHaveType('Prophecy\Argument\Token\StringContainsToken');
    }

    function it_implements_TokenInterface()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Argument\Token\TokenInterface');
    }

    function it_holds_value()
    {
        $this->getValue()->shouldReturn('a substring');
    }

    function it_is_not_last()
    {
        $this->shouldNotBeLast();
    }

    function it_scores_6_if_the_argument_contains_the_value()
    {
        $this->scoreArgument('Argument containing a substring')->shouldReturn(6);
    }

    function it_does_not_score_if_the_argument_does_not_contain_the_value()
    {
        $this->scoreArgument('Argument will not match')->shouldReturn(false);
    }

    function its_string_representation_shows_substring()
    {
        $this->__toString()->shouldReturn('contains("a substring")');
    }
}
