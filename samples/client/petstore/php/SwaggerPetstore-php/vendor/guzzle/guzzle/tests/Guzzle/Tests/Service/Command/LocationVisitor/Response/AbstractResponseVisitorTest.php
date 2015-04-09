<?php

namespace Guzzle\Tests\Service\Command\LocationVisitor\Response;

use Guzzle\Tests\Service\Mock\Command\MockCommand;
use Guzzle\Http\Message\Response;

abstract class AbstractResponseVisitorTest extends \Guzzle\Tests\GuzzleTestCase
{
    /** @var Response */
    protected $response;

    /** @var MockCommand */
    protected $command;

    /** @var array */
    protected $value;

    public function setUp()
    {
        $this->value = array();
        $this->command = new MockCommand();
        $this->response = new Response(200, array(
            'X-Foo'          => 'bar',
            'Content-Length' => 3,
            'Content-Type'   => 'text/plain'
        ), 'Foo');
    }
}
