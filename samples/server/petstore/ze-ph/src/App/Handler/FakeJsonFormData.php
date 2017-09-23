<?php

namespace App\Handler;

use Articus\PathHandler\Operation;
use Articus\PathHandler\Annotation as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;

class FakeJsonFormData implements Operation\GetInterface
{
    /**
     * test json serialization of form data
     * @param ServerRequestInterface $request
     *
     * @throws PHException\HttpCode 500 if the method is not implemented
     */
    public function handleGet(ServerRequestInterface $request)
    {
        //TODO implement method
        throw new PHException\HttpCode(500, "Not implemented");
    }
}
