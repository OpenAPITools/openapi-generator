<?php

namespace spec\Prophecy;

use PhpSpec\ObjectBehavior;

class ArgumentSpec extends ObjectBehavior
{
    function it_has_a_shortcut_for_exact_argument_token()
    {
        $token = $this->exact(42);
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\ExactValueToken');
        $token->getValue()->shouldReturn(42);
    }

    function it_has_a_shortcut_for_any_argument_token()
    {
        $token = $this->any();
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\AnyValueToken');
    }

    function it_has_a_shortcut_for_multiple_arguments_token()
    {
        $token = $this->cetera();
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\AnyValuesToken');
    }

    function it_has_a_shortcut_for_type_token()
    {
        $token = $this->type('integer');
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\TypeToken');
    }

    function it_has_a_shortcut_for_callback_token()
    {
        $token = $this->that('get_class');
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\CallbackToken');
    }

    function it_has_a_shortcut_for_object_state_token()
    {
        $token = $this->which('getName', 'everzet');
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\ObjectStateToken');
    }

    function it_has_a_shortcut_for_logical_and_token()
    {
        $token = $this->allOf('integer', 5);
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\LogicalAndToken');
    }

    function it_has_a_shortcut_for_array_count_token()
    {
        $token = $this->size(5);
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\ArrayCountToken');
    }

    function it_has_a_shortcut_for_array_entry_token()
    {
        $token = $this->withEntry('key', 'value');
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\ArrayEntryToken');
    }

    function it_has_a_shortcut_for_array_every_entry_token()
    {
        $token = $this->withEveryEntry('value');
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\ArrayEveryEntryToken');
    }

    function it_has_a_shortcut_for_identical_value_token()
    {
        $token = $this->is('value');
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\IdenticalValueToken');
    }

    function it_has_a_shortcut_for_array_entry_token_matching_any_key()
    {
        $token = $this->containing('value');
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\ArrayEntryToken');
        $token->getKey()->shouldHaveType('Prophecy\Argument\Token\AnyValueToken');
    }

    function it_has_a_shortcut_for_array_entry_token_matching_any_value()
    {
        $token = $this->withKey('key');
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\ArrayEntryToken');
        $token->getValue()->shouldHaveType('Prophecy\Argument\Token\AnyValueToken');
    }

    function it_has_a_shortcut_for_logical_not_token()
    {
        $token = $this->not('kagux');
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\LogicalNotToken');
    }

    function it_has_a_shortcut_for_string_contains_token()
    {
        $token = $this->containingString('string');
        $token->shouldBeAnInstanceOf('Prophecy\Argument\Token\StringContainsToken');
    }
}
