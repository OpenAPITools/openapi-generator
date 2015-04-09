<?php

namespace Guzzle\Tests\Mock;

use Guzzle\Plugin\ErrorResponse\ErrorResponseExceptionInterface;
use Guzzle\Service\Command\CommandInterface;
use Guzzle\Http\Message\Response;

class ErrorResponseMock extends \Exception implements ErrorResponseExceptionInterface
{
    public $command;
    public $response;

    public static function fromCommand(CommandInterface $command, Response $response)
    {
        return new self($command, $response);
    }

    public function __construct($command, $response)
    {
        $this->command = $command;
        $this->response = $response;
        $this->message = 'Error from ' . $response;
    }
}
