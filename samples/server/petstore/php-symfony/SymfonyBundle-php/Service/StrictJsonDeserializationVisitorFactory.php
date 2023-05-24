<?php

declare(strict_types=1);

namespace OpenAPI\Server\Service;

use JMS\Serializer\Visitor\DeserializationVisitorInterface;
use JMS\Serializer\Visitor\Factory\DeserializationVisitorFactory;

final class StrictJsonDeserializationVisitorFactory implements DeserializationVisitorFactory
{
    private int $options = 0;

    private int $depth = 512;

    public function getVisitor(): DeserializationVisitorInterface
    {
        return new StrictJsonDeserializationVisitor($this->options, $this->depth);
    }

    public function setOptions(int $options): self
    {
        $this->options = $options;

        return $this;
    }

    public function setDepth(int $depth): self
    {
        $this->depth = $depth;

        return $this;
    }
}
