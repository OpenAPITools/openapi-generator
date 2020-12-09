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
 * @PHA\Route(pattern="/store/order")
 */
class StoreOrder
{
    /**
     * Place an order for a pet
     * @PHA\Post()
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaRange="application/json")
     * @PHA\Attribute(name=PHAttribute\Transfer::class, options={"type":\App\DTO\Order::class,"objectAttr":"bodyData"})
     * TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Producer(name=PHProducer\Transfer::class, mediaType="application/xml")
     * TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Producer(name=PHProducer\Transfer::class, mediaType="application/json")
     * @param ServerRequestInterface $request
     *
     * @throws PHException\HttpCode 501 if the method is not implemented
     *
     * @return \App\DTO\Order
     */
    public function placeOrder(ServerRequestInterface $request): \App\DTO\Order
    {
        //TODO implement method
        /** @var \App\DTO\Order $bodyData */
        $bodyData = $request->getAttribute("bodyData");
        throw new PHException\HttpCode(501, "Not implemented");
    }
}
