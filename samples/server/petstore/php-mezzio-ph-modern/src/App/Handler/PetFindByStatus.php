<?php
declare(strict_types=1);

namespace App\Handler;

use Articus\PathHandler\PhpAttribute as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;

#[PHA\Route("/pet/findByStatus")]
class PetFindByStatus
{
    /**
     * Finds Pets by status
     */
    #[PHA\Get()]
    #[PHA\Attribute(PHAttribute\Transfer::class, [
        "type" => \App\DTO\FindPetsByStatusQueryData::class,
        "objectAttr" => "queryData",
        "source" => PHAttribute\Transfer::SOURCE_GET
    ])]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/xml", PHProducer\Transfer::class)]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/json", PHProducer\Transfer::class)]
    public function findPetsByStatus(ServerRequestInterface $request): \App\DTO\Collection19
    {
        //TODO implement method
        /** @var \App\DTO\FindPetsByStatusQueryData $queryData */
        $queryData = $request->getAttribute("queryData");
        throw new PHException\HttpCode(501, "Not implemented");
    }
}
