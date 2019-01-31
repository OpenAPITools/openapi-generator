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
 * @PHA\Route(pattern="/user/createWithList")
 */
class UserCreateWithList
{
    /**
     * Creates list of users with given input array
     * @PHA\Post()
     * TODO check if attribute is valid and can handle your container type
     * @PHA\Attribute(name=PHAttribute\Transfer::class, options={"type":"\App\DTO\User[]","objectAttr":"bodyData"})
     * @param ServerRequestInterface $request
     *
     * @throws PHException\HttpCode 501 if the method is not implemented
     */
    public function createUsersWithListInput(ServerRequestInterface $request)
    {
        //TODO implement method
        /** @var \App\DTO\User[] $bodyData */
        $bodyData = $request->getAttribute("bodyData");
        throw new PHException\HttpCode(501, "Not implemented");
    }
}
