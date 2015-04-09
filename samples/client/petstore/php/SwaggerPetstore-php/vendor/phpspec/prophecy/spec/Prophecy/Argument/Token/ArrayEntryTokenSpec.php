<?php

namespace spec\Prophecy\Argument\Token;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;
use Prophecy\Exception\InvalidArgumentException;

class ArrayEntryTokenSpec extends ObjectBehavior
{
    /**
     * @param \Prophecy\Argument\Token\TokenInterface $key
     * @param \Prophecy\Argument\Token\TokenInterface $value
     */
    function let($key, $value)
    {
        $this->beConstructedWith($key, $value);
    }

    function it_implements_TokenInterface()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Argument\Token\TokenInterface');
    }

    function it_is_not_last()
    {
        $this->shouldNotBeLast();
    }

    function it_holds_key_and_value($key, $value)
    {
        $this->getKey()->shouldBe($key);
        $this->getValue()->shouldBe($value);
    }

    function its_string_representation_tells_that_its_an_array_containing_the_key_value_pair($key, $value)
    {
        $key->__toString()->willReturn('key');
        $value->__toString()->willReturn('value');
        $this->__toString()->shouldBe('[..., key => value, ...]');
    }

    /**
     * @param \Prophecy\Argument\Token\TokenInterface $key
     * @param \stdClass                               $object
     */
    function it_wraps_non_token_value_into_ExactValueToken($key, $object)
    {
        $this->beConstructedWith($key, $object);
        $this->getValue()->shouldHaveType('\Prophecy\Argument\Token\ExactValueToken');
    }

    /**
     * @param \stdClass                               $object
     * @param \Prophecy\Argument\Token\TokenInterface $value
     */
    function it_wraps_non_token_key_into_ExactValueToken($object, $value)
    {
        $this->beConstructedWith($object, $value);
        $this->getKey()->shouldHaveType('\Prophecy\Argument\Token\ExactValueToken');
    }

    function it_scores_array_half_of_combined_scores_from_key_and_value_tokens($key, $value)
    {
        $key->scoreArgument('key')->willReturn(4);
        $value->scoreArgument('value')->willReturn(6);
        $this->scoreArgument(array('key'=>'value'))->shouldBe(5);
    }

    /**
     * @param \Prophecy\Argument\Token\TokenInterface $key
     * @param \Prophecy\Argument\Token\TokenInterface $value
     * @param \Iterator                               $object
     */
    function it_scores_traversable_object_half_of_combined_scores_from_key_and_value_tokens($key, $value, $object)
    {
        $object->current()->will(function () use ($object) {
            $object->valid()->willReturn(false);

            return 'value';
        });
        $object->key()->willReturn('key');
        $object->rewind()->willReturn(null);
        $object->next()->willReturn(null);
        $object->valid()->willReturn(true);
        $key->scoreArgument('key')->willReturn(6);
        $value->scoreArgument('value')->willReturn(2);
        $this->scoreArgument($object)->shouldBe(4);
    }

    /**
     * @param \Prophecy\Argument\Token\AnyValuesToken $key
     * @param \Prophecy\Argument\Token\TokenInterface $value
     * @param \ArrayAccess                            $object
     */
    function it_throws_exception_during_scoring_of_array_accessible_object_if_key_is_not_ExactValueToken($key, $value, $object)
    {
        $key->__toString()->willReturn('any_token');
        $this->beConstructedWith($key,$value);
        $errorMessage = 'You can only use exact value tokens to match key of ArrayAccess object'.PHP_EOL.
                        'But you used `any_token`.';
        $this->shouldThrow(new InvalidArgumentException($errorMessage))->duringScoreArgument($object);
    }

    /**
     * @param \Prophecy\Argument\Token\ExactValueToken $key
     * @param \Prophecy\Argument\Token\TokenInterface  $value
     * @param \ArrayAccess                             $object
     */
    function it_scores_array_accessible_object_half_of_combined_scores_from_key_and_value_tokens($key, $value, $object)
    {
        $object->offsetExists('key')->willReturn(true);
        $object->offsetGet('key')->willReturn('value');
        $key->getValue()->willReturn('key');
        $key->scoreArgument('key')->willReturn(3);
        $value->scoreArgument('value')->willReturn(1);
        $this->scoreArgument($object)->shouldBe(2);
    }

    /**
     * @param \Prophecy\Argument\Token\AnyValuesToken $key
     * @param \Prophecy\Argument\Token\TokenInterface $value
     * @param \ArrayIterator                          $object
     */
    function it_accepts_any_key_token_type_to_score_object_that_is_both_traversable_and_array_accessible($key, $value, $object)
    {
        $this->beConstructedWith($key, $value);
        $object->current()->will(function () use ($object) {
            $object->valid()->willReturn(false);

            return 'value';
        });
        $object->key()->willReturn('key');
        $object->rewind()->willReturn(null);
        $object->next()->willReturn(null);
        $object->valid()->willReturn(true);
        $this->shouldNotThrow(new InvalidArgumentException)->duringScoreArgument($object);
    }

    function it_does_not_score_if_argument_is_neither_array_nor_traversable_nor_array_accessible()
    {
        $this->scoreArgument('string')->shouldBe(false);
        $this->scoreArgument(new \stdClass)->shouldBe(false);
    }

    function it_does_not_score_empty_array()
    {
        $this->scoreArgument(array())->shouldBe(false);
    }

    function it_does_not_score_array_if_key_and_value_tokens_do_not_score_same_entry($key, $value)
    {
        $argument = array(1 => 'foo', 2 => 'bar');
        $key->scoreArgument(1)->willReturn(true);
        $key->scoreArgument(2)->willReturn(false);
        $value->scoreArgument('foo')->willReturn(false);
        $value->scoreArgument('bar')->willReturn(true);
        $this->scoreArgument($argument)->shouldBe(false);
    }

    /**
     * @param \Iterator $object
     */
    function it_does_not_score_traversable_object_without_entries($object)
    {
        $object->rewind()->willReturn(null);
        $object->next()->willReturn(null);
        $object->valid()->willReturn(false);
        $this->scoreArgument($object)->shouldBe(false);
    }

    /**
     * @param \Prophecy\Argument\Token\TokenInterface $key
     * @param \Prophecy\Argument\Token\TokenInterface $value
     * @param \Iterator                               $object
     */
    function it_does_not_score_traversable_object_if_key_and_value_tokens_do_not_score_same_entry($key, $value, $object)
    {
        $object->current()->willReturn('foo');
        $object->current()->will(function () use ($object) {
            $object->valid()->willReturn(false);

            return 'bar';
        });
        $object->key()->willReturn(1);
        $object->key()->willReturn(2);
        $object->rewind()->willReturn(null);
        $object->next()->willReturn(null);
        $object->valid()->willReturn(true);
        $key->scoreArgument(1)->willReturn(true);
        $key->scoreArgument(2)->willReturn(false);
        $value->scoreArgument('foo')->willReturn(false);
        $value->scoreArgument('bar')->willReturn(true);
        $this->scoreArgument($object)->shouldBe(false);
    }

    /**
     * @param \Prophecy\Argument\Token\ExactValueToken $key
     * @param \ArrayAccess                             $object
     */
    function it_does_not_score_array_accessible_object_if_it_has_no_offset_with_key_token_value($key, $object)
    {
        $object->offsetExists('key')->willReturn(false);
        $key->getValue()->willReturn('key');
        $this->scoreArgument($object)->shouldBe(false);
    }

    /**
     * @param \Prophecy\Argument\Token\ExactValueToken $key
     * @param \Prophecy\Argument\Token\TokenInterface  $value
     * @param \ArrayAccess                             $object
     */
    function it_does_not_score_array_accessible_object_if_key_and_value_tokens_do_not_score_same_entry($key, $value, $object)
    {
        $object->offsetExists('key')->willReturn(true);
        $object->offsetGet('key')->willReturn('value');
        $key->getValue()->willReturn('key');
        $value->scoreArgument('value')->willReturn(false);
        $key->scoreArgument('key')->willReturn(true);
        $this->scoreArgument($object)->shouldBe(false);
    }

    function its_score_is_capped_at_8($key, $value)
    {
        $key->scoreArgument('key')->willReturn(10);
        $value->scoreArgument('value')->willReturn(10);
        $this->scoreArgument(array('key'=>'value'))->shouldBe(8);
    }
}
