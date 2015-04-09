<?php

namespace spec\Prophecy\Argument\Token;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument\Token\TokenInterface;

class LogicalNotTokenSpec extends ObjectBehavior
{
    /**
     * @param \Prophecy\Argument\Token\TokenInterface $token
     */
    function let($token)
    {
        $this->beConstructedWith($token);
    }

    function it_implements_TokenInterface()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Argument\Token\TokenInterface');
    }

    function it_holds_originating_token($token)
    {
        $this->getOriginatingToken()->shouldReturn($token);
    }

    function it_has_simple_string_representation($token)
    {
        $token->__toString()->willReturn('value');
        $this->__toString()->shouldBe('not(value)');
    }

    function it_wraps_non_token_argument_into_ExactValueToken()
    {
        $this->beConstructedWith(5);
        $token = $this->getOriginatingToken();
        $token->shouldhaveType('Prophecy\Argument\Token\ExactValueToken');
        $token->getValue()->shouldBe(5);
    }

    function it_scores_4_if_preset_token_does_not_match_the_argument($token)
    {
        $token->scoreArgument('argument')->willReturn(false);
        $this->scoreArgument('argument')->shouldBe(4);
    }

    function it_does_not_score_if_preset_token_matches_argument($token)
    {
        $token->scoreArgument('argument')->willReturn(5);
        $this->scoreArgument('argument')->shouldBe(false);
    }

    function it_is_last_if_preset_token_is_last($token)
    {
        $token->isLast()->willReturn(true);
        $this->shouldBeLast();
    }

    function it_is_not_last_if_preset_token_is_not_last($token)
    {
        $token->isLast()->willReturn(false);
        $this->shouldNotBeLast();
    }
}
