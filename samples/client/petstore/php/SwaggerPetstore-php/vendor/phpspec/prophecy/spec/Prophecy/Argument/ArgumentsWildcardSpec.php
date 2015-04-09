<?php

namespace spec\Prophecy\Argument;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument\Token\TokenInterface;

class ArgumentsWildcardSpec extends ObjectBehavior
{
    /**
     * @param \stdClass $object
     */
    function it_wraps_non_token_arguments_into_ExactValueToken($object)
    {
        $this->beConstructedWith(array(42, 'zet', $object));

        $class = get_class($object->getWrappedObject());
        $hash  = spl_object_hash($object->getWrappedObject());

        $this->__toString()->shouldReturn("exact(42), exact(\"zet\"), exact($class:$hash Object (\n    'objectProphecy' => Prophecy\Prophecy\ObjectProphecy Object (*Prophecy*)\n))");
    }

    /**
     * @param \Prophecy\Argument\Token\TokenInterface $token1
     * @param \Prophecy\Argument\Token\TokenInterface $token2
     * @param \Prophecy\Argument\Token\TokenInterface $token3
     */
    function it_generates_string_representation_from_all_tokens_imploded($token1, $token2, $token3)
    {
        $token1->__toString()->willReturn('token_1');
        $token2->__toString()->willReturn('token_2');
        $token3->__toString()->willReturn('token_3');

        $this->beConstructedWith(array($token1, $token2, $token3));
        $this->__toString()->shouldReturn('token_1, token_2, token_3');
    }

    function it_exposes_list_of_tokens(TokenInterface $token)
    {
        $this->beConstructedWith(array($token));

        $this->getTokens()->shouldReturn(array($token));
    }

    function it_returns_score_of_1_if_there_are_no_tokens_and_arguments()
    {
        $this->beConstructedWith(array());

        $this->scoreArguments(array())->shouldReturn(1);
    }

    /**
     * @param \Prophecy\Argument\Token\TokenInterface $token1
     * @param \Prophecy\Argument\Token\TokenInterface $token2
     * @param \Prophecy\Argument\Token\TokenInterface $token3
     */
    function it_should_return_match_score_based_on_all_tokens_score($token1, $token2, $token3)
    {
        $token1->scoreArgument('one')->willReturn(3);
        $token1->isLast()->willReturn(false);
        $token2->scoreArgument(2)->willReturn(5);
        $token2->isLast()->willReturn(false);
        $token3->scoreArgument($obj = new \stdClass())->willReturn(10);
        $token3->isLast()->willReturn(false);

        $this->beConstructedWith(array($token1, $token2, $token3));
        $this->scoreArguments(array('one', 2, $obj))->shouldReturn(18);
    }

    /**
     * @param \Prophecy\Argument\Token\TokenInterface $token1
     * @param \Prophecy\Argument\Token\TokenInterface $token2
     * @param \Prophecy\Argument\Token\TokenInterface $token3
     */
    function it_returns_false_if_there_is_less_arguments_than_tokens($token1, $token2, $token3)
    {
        $token1->scoreArgument('one')->willReturn(3);
        $token1->isLast()->willReturn(false);
        $token2->scoreArgument(2)->willReturn(5);
        $token2->isLast()->willReturn(false);
        $token3->scoreArgument(null)->willReturn(false);
        $token3->isLast()->willReturn(false);

        $this->beConstructedWith(array($token1, $token2, $token3));
        $this->scoreArguments(array('one', 2))->shouldReturn(false);
    }

    /**
     * @param \Prophecy\Argument\Token\TokenInterface $token1
     * @param \Prophecy\Argument\Token\TokenInterface $token2
     * @param \Prophecy\Argument\Token\TokenInterface $token3
     */
    function it_returns_false_if_there_is_less_tokens_than_arguments($token1, $token2, $token3)
    {
        $token1->scoreArgument('one')->willReturn(3);
        $token1->isLast()->willReturn(false);
        $token2->scoreArgument(2)->willReturn(5);
        $token2->isLast()->willReturn(false);
        $token3->scoreArgument($obj = new \stdClass())->willReturn(10);
        $token3->isLast()->willReturn(false);

        $this->beConstructedWith(array($token1, $token2, $token3));
        $this->scoreArguments(array('one', 2, $obj, 4))->shouldReturn(false);
    }

    /**
     * @param \Prophecy\Argument\Token\TokenInterface $token1
     * @param \Prophecy\Argument\Token\TokenInterface $token2
     * @param \Prophecy\Argument\Token\TokenInterface $token3
     */
    function it_should_return_false_if_one_of_the_tokens_returns_false($token1, $token2, $token3)
    {
        $token1->scoreArgument('one')->willReturn(3);
        $token1->isLast()->willReturn(false);
        $token2->scoreArgument(2)->willReturn(false);
        $token2->isLast()->willReturn(false);
        $token3->scoreArgument($obj = new \stdClass())->willReturn(10);
        $token3->isLast()->willReturn(false);

        $this->beConstructedWith(array($token1, $token2, $token3));
        $this->scoreArguments(array('one', 2, $obj))->shouldReturn(false);
    }

    /**
     * @param \Prophecy\Argument\Token\TokenInterface $token1
     * @param \Prophecy\Argument\Token\TokenInterface $token2
     * @param \Prophecy\Argument\Token\TokenInterface $token3
     */
    function it_should_calculate_score_until_last_token($token1, $token2, $token3)
    {
        $token1->scoreArgument('one')->willReturn(3);
        $token1->isLast()->willReturn(false);

        $token2->scoreArgument(2)->willReturn(7);
        $token2->isLast()->willReturn(true);

        $token3->scoreArgument($obj = new \stdClass())->willReturn(10);
        $token3->isLast()->willReturn(false);

        $this->beConstructedWith(array($token1, $token2, $token3));
        $this->scoreArguments(array('one', 2, $obj))->shouldReturn(10);
    }
}
