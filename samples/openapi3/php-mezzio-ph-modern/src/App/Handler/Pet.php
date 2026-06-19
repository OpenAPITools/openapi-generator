<?php
declare(strict_types=1);

namespace App\Handler;

use Articus\PathHandler\PhpAttribute as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;

#[PHA\Route("/pet")]
class Pet
{
    /**
     * Add a new pet to the store
     */
    #[PHA\Post()]
    // TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Consumer("application/json", PHConsumer\Json::class)]
    // TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Consumer("application/xml", PHConsumer\Json::class)]
    #[PHA\Attribute(PHAttribute\Transfer::class, ["type" => \App\DTO\Pet::class, "objectAttr" => "bodyData"])]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/xml", PHProducer\Transfer::class)]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/json", PHProducer\Transfer::class)]
    public function addPet(ServerRequestInterface $request): \App\DTO\Pet
    {
        //TODO implement method
        /** @var \App\DTO\Pet $bodyData */
        $bodyData = $request->getAttribute("bodyData");
        throw new PHException\HttpCode(501, "Not implemented");
    }
    /**
     * Update an existing pet
     */
    #[PHA\Put()]
    // TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Consumer("application/json", PHConsumer\Json::class)]
    // TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Consumer("application/xml", PHConsumer\Json::class)]
    #[PHA\Attribute(PHAttribute\Transfer::class, ["type" => \App\DTO\Pet::class, "objectAttr" => "bodyData"])]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/xml", PHProducer\Transfer::class)]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/json", PHProducer\Transfer::class)]
    public function updatePet(ServerRequestInterface $request): \App\DTO\Pet
    {
        //TODO implement method
        /** @var \App\DTO\Pet $bodyData */
        $bodyData = $request->getAttribute("bodyData");
        throw new PHException\HttpCode(501, "Not implemented");
    }
}
