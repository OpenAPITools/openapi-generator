<?php
declare(strict_types=1);

namespace App\Handler;

use Articus\PathHandler\PhpAttribute as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;

#[PHA\Route("/user/createWithList")]
class UserCreateWithList
{
    /**
     * Creates list of users with given input array
     */
    #[PHA\Post()]
    // TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Consumer("application/json", PHConsumer\Json::class)]
    #[PHA\Attribute(PHAttribute\Transfer::class, ["type" => \App\DTO\Collection36::class, "objectAttr" => "bodyData"])]
    public function createUsersWithListInput(ServerRequestInterface $request)
    {
        //TODO implement method
        /** @var \App\DTO\Collection36 $bodyData */
        $bodyData = $request->getAttribute("bodyData");
        throw new PHException\HttpCode(501, "Not implemented");
    }
}
