<?php

namespace App\Handler;

use Articus\PathHandler\Operation;
use Articus\PathHandler\Annotation as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;

/**
 * @PHA\Route(pattern="/pet")
 */
class Pet implements Operation\PostInterface, Operation\PutInterface
{
    /**
     * Add a new pet to the store
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaType="application/json")
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaType="application/xml")
     * @PHA\Attribute(name=PHAttribute\Transfer::class, options={"type":\App\DTO\Pet::class,"objectAttr":"bodyData"})
     * @param ServerRequestInterface $request
     *
     * @throws PHException\HttpCode 500 if the method is not implemented
     */
    public function handlePost(ServerRequestInterface $request)
    {
        //TODO implement method
        /** @var \App\DTO\Pet $bodyData */
        $bodyData = $request->getAttribute("bodyData");
        throw new PHException\HttpCode(500, "Not implemented");
    }
    /**
     * Update an existing pet
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaType="application/json")
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaType="application/xml")
     * @PHA\Attribute(name=PHAttribute\Transfer::class, options={"type":\App\DTO\Pet::class,"objectAttr":"bodyData"})
     * @param ServerRequestInterface $request
     *
     * @throws PHException\HttpCode 500 if the method is not implemented
     */
    public function handlePut(ServerRequestInterface $request)
    {
        //TODO implement method
        /** @var \App\DTO\Pet $bodyData */
        $bodyData = $request->getAttribute("bodyData");
        throw new PHException\HttpCode(500, "Not implemented");
    }
}
