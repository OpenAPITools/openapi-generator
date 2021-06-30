<?php
declare(strict_types=1);

namespace App\Handler;

use Articus\PathHandler\PhpAttribute as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;

#[PHA\Route("/store/order/{orderId}")]
class StoreOrderOrderId
{
    /**
     * Delete purchase order by ID
     */
    #[PHA\Delete()]
    public function deleteOrder(ServerRequestInterface $request)
    {
        //TODO implement method
        throw new PHException\HttpCode(501, "Not implemented");
    }
    /**
     * Find purchase order by ID
     */
    #[PHA\Get()]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/xml", PHProducer\Transfer::class)]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/json", PHProducer\Transfer::class)]
    public function getOrderById(ServerRequestInterface $request): \App\DTO\Order
    {
        //TODO implement method
        throw new PHException\HttpCode(501, "Not implemented");
    }
}
