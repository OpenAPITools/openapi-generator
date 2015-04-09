<?php

namespace Guzzle\Tests\Mock;

use Guzzle\Service\Command\ResponseClassInterface;
use Guzzle\Service\Command\OperationCommand;

class CustomResponseModel implements ResponseClassInterface
{
    public $command;

    public static function fromCommand(OperationCommand $command)
    {
        return new self($command);
    }

    public function __construct($command)
    {
        $this->command = $command;
    }
}
