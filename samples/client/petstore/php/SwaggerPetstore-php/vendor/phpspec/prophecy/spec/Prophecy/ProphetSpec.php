<?php

namespace spec\Prophecy;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class ProphetSpec extends ObjectBehavior
{
    /**
     * @param \Prophecy\Doubler\Doubler                   $doubler
     * @param \Prophecy\Prophecy\ProphecySubjectInterface $double
     */
    function let($doubler, $double)
    {
        $doubler->double(null, array())->willReturn($double);

        $this->beConstructedWith($doubler);
    }

    function it_constructs_new_prophecy_on_prophesize_call()
    {
        $prophecy = $this->prophesize();
        $prophecy->shouldBeAnInstanceOf('Prophecy\Prophecy\ObjectProphecy');
    }

    /**
     * @param \Prophecy\Prophecy\ProphecySubjectInterface $newDouble
     */
    function it_constructs_new_prophecy_with_parent_class_if_specified($doubler, $newDouble)
    {
        $doubler->double(Argument::any(), array())->willReturn($newDouble);

        $this->prophesize('Prophecy\Prophet')->reveal()->shouldReturn($newDouble);
    }

    /**
     * @param \Prophecy\Prophecy\ProphecySubjectInterface $newDouble
     */
    function it_constructs_new_prophecy_with_interface_if_specified($doubler, $newDouble)
    {
        $doubler->double(null, Argument::any())->willReturn($newDouble);

        $this->prophesize('ArrayAccess')->reveal()->shouldReturn($newDouble);
    }

    function it_exposes_all_created_prophecies_through_getter()
    {
        $prophecy1 = $this->prophesize();
        $prophecy2 = $this->prophesize();

        $this->getProphecies()->shouldReturn(array($prophecy1, $prophecy2));
    }

    function it_does_nothing_during_checkPredictions_call_if_no_predictions_defined()
    {
        $this->checkPredictions()->shouldReturn(null);
    }

    /**
     * @param \Prophecy\Prophecy\MethodProphecy    $method1
     * @param \Prophecy\Prophecy\MethodProphecy    $method2
     * @param \Prophecy\Argument\ArgumentsWildcard $arguments1
     * @param \Prophecy\Argument\ArgumentsWildcard $arguments2
     */
    function it_throws_AggregateException_if_defined_predictions_fail(
        $method1, $method2, $arguments1, $arguments2
    )
    {
        $method1->getMethodName()->willReturn('getName');
        $method1->getArgumentsWildcard()->willReturn($arguments1);
        $method1->checkPrediction()->willReturn(null);

        $method2->getMethodName()->willReturn('isSet');
        $method2->getArgumentsWildcard()->willReturn($arguments2);
        $method2->checkPrediction()->willThrow(
            'Prophecy\Exception\Prediction\AggregateException'
        );

        $this->prophesize()->addMethodProphecy($method1);
        $this->prophesize()->addMethodProphecy($method2);

        $this->shouldThrow('Prophecy\Exception\Prediction\AggregateException')
            ->duringCheckPredictions();
    }

    function it_exposes_doubler_through_getter($doubler)
    {
        $this->getDoubler()->shouldReturn($doubler);
    }
}
