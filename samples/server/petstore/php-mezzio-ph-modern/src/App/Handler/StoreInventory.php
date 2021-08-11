<?php
declare(strict_types=1);

namespace App\Handler;

use Articus\PathHandler\PhpAttribute as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;

#[PHA\Route("/store/inventory")]
class StoreInventory
{
    /**
     * Returns pet inventories by status
     */
    #[PHA\Get()]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/json", PHProducer\Transfer::class)]
    public function getInventory(ServerRequestInterface $request): \App\DTO\Collection34
    {
        //TODO implement method
        throw new PHException\HttpCode(501, "Not implemented");
    }
}
