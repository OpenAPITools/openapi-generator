<?php
declare(strict_types=1);

namespace App\Handler;

use Articus\PathHandler\Annotation as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;

/**
 * @PHA\Route(pattern="/pet")
 */
class Pet
{
    /**
     * Add a new pet to the store
     * @PHA\Post()
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaRange="application/json")
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaRange="application/xml")
     * @PHA\Attribute(name=PHAttribute\Transfer::class, options={"type":\App\DTO\Pet::class,"objectAttr":"bodyData"})
     * TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Producer(name=PHProducer\Transfer::class, mediaType="application/xml")
     * TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Producer(name=PHProducer\Transfer::class, mediaType="application/json")
     * @param ServerRequestInterface $request
     *
     * @throws PHException\HttpCode 501 if the method is not implemented
     *
     * @return \App\DTO\Pet
     */
    public function addPet(ServerRequestInterface $request): \App\DTO\Pet
    {
        //TODO implement method
        /** @var \App\DTO\Pet $bodyData */
        $bodyData = $request->getAttribute("bodyData");
        throw new PHException\HttpCode(501, "Not implemented");
    }
    /**
     * Update an existing pet
     * @PHA\Put()
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaRange="application/json")
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaRange="application/xml")
     * @PHA\Attribute(name=PHAttribute\Transfer::class, options={"type":\App\DTO\Pet::class,"objectAttr":"bodyData"})
     * TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Producer(name=PHProducer\Transfer::class, mediaType="application/xml")
     * TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Producer(name=PHProducer\Transfer::class, mediaType="application/json")
     * @param ServerRequestInterface $request
     *
     * @throws PHException\HttpCode 501 if the method is not implemented
     *
     * @return \App\DTO\Pet
     */
    public function updatePet(ServerRequestInterface $request): \App\DTO\Pet
    {
        //TODO implement method
        /** @var \App\DTO\Pet $bodyData */
        $bodyData = $request->getAttribute("bodyData");
        throw new PHException\HttpCode(501, "Not implemented");
    }
}
