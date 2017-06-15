<?php

namespace App\Handler;

use Articus\PathHandler\Operation;
use Articus\PathHandler\Annotation as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;


class FakeOuterBoolean implements Operation\PostInterface
{
    /**
     * @PHA\Attribute(name=PHAttribute\Transfer::class, options={"type":\App\DTO\OuterBoolean::class,"objectAttr":"body"})
     * @return \App\DTO\OuterBoolean
     */
    public function handlePost(ServerRequestInterface $request)
    {
        //TODO implement method
        /** @var \App\DTO\OuterBoolean $body */
        $body = $request->getAttribute("body");
        throw new PHException\HttpCode(500, "Not implemented");
    }
}
