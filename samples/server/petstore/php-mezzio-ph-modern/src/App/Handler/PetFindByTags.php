<?php
declare(strict_types=1);

namespace App\Handler;

use Articus\PathHandler\PhpAttribute as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;

#[PHA\Route("/pet/findByTags")]
class PetFindByTags
{
    /**
     * Finds Pets by tags
     */
    #[PHA\Get()]
    #[PHA\Attribute(PHAttribute\Transfer::class, [
        "type" => \App\DTO\FindPetsByTagsQueryData::class,
        "objectAttr" => "queryData",
        "source" => PHAttribute\Transfer::SOURCE_GET
    ])]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/xml", PHProducer\Transfer::class)]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/json", PHProducer\Transfer::class)]
    public function findPetsByTags(ServerRequestInterface $request): \App\DTO\Collection26
    {
        //TODO implement method
        /** @var \App\DTO\FindPetsByTagsQueryData $queryData */
        $queryData = $request->getAttribute("queryData");
        throw new PHException\HttpCode(501, "Not implemented");
    }
}
