<?php
declare(strict_types=1);

namespace App\Handler;

use Articus\PathHandler\PhpAttribute as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;

#[PHA\Route("/user/{username}")]
class UserUsername
{
    /**
     * Delete user
     */
    #[PHA\Delete()]
    public function deleteUser(ServerRequestInterface $request)
    {
        //TODO implement method
        throw new PHException\HttpCode(501, "Not implemented");
    }
    /**
     * Get user by user name
     */
    #[PHA\Get()]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/xml", PHProducer\Transfer::class)]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/json", PHProducer\Transfer::class)]
    public function getUserByName(ServerRequestInterface $request): \App\DTO\User
    {
        //TODO implement method
        throw new PHException\HttpCode(501, "Not implemented");
    }
    /**
     * Updated user
     */
    #[PHA\Put()]
    // TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Consumer("application/json", PHConsumer\Json::class)]
    #[PHA\Attribute(PHAttribute\Transfer::class, ["type" => \App\DTO\User::class, "objectAttr" => "bodyData"])]
    public function updateUser(ServerRequestInterface $request)
    {
        //TODO implement method
        /** @var \App\DTO\User $bodyData */
        $bodyData = $request->getAttribute("bodyData");
        throw new PHException\HttpCode(501, "Not implemented");
    }
}
