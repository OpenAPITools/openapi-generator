<?php
/*
 * Copyright 2017 Dmitriy Simushev
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

declare(strict_types=1);

namespace OpenAPI\Server\Service;

use JMS\Serializer\JsonDeserializationVisitor;
use JMS\Serializer\GraphNavigatorInterface;
use JMS\Serializer\Metadata\ClassMetadata;
use JMS\Serializer\Metadata\PropertyMetadata;
use JMS\Serializer\Visitor\DeserializationVisitorInterface;

class StrictJsonDeserializationVisitor implements DeserializationVisitorInterface
{
    protected JsonDeserializationVisitor $jsonDeserializationVisitor;

    public function __construct(
        int $options = 0,
        int $depth = 512
    ) {
        $this->jsonDeserializationVisitor = new JsonDeserializationVisitor($options, $depth);
    }

    /**
     * {@inheritdoc}
     */
    public function visitNull($data, array $type) 
    {
        return $this->jsonDeserializationVisitor->visitNull($data, $type);
    }

    /**
     * {@inheritdoc}
     */
    public function visitString($data, array $type): string
    {
        if (!is_string($data)) {
            throw TypeMismatchException::fromValue('string', $data);
        }

        return $this->jsonDeserializationVisitor->visitString($data, $type);
    }

    /**
     * {@inheritdoc}
     */
    public function visitBoolean($data, array $type): bool
    {
        if (!is_bool($data)) {
            throw TypeMismatchException::fromValue('boolean', $data);
        }

        return $this->jsonDeserializationVisitor->visitBoolean($data, $type);
    }

    /**
     * {@inheritdoc}
     */
    public function visitInteger($data, array $type): int
    {
        if (!is_int($data)) {
            throw TypeMismatchException::fromValue('integer', $data);
        }

        return $this->jsonDeserializationVisitor->visitInteger($data, $type);
    }

    /**
     * {@inheritdoc}
     */
    public function visitDouble($data, array $type): float
    {
        if (!is_float($data) && !is_integer($data)) {
            throw TypeMismatchException::fromValue('double', $data);
        }

        return $this->jsonDeserializationVisitor->visitDouble($data, $type);
    }

    /**
     * {@inheritdoc}
     */
    public function visitArray($data, array $type): array
    {
        return $this->jsonDeserializationVisitor->visitArray($data, $type);
    }

    /**
     * {@inheritdoc}
     */
    public function visitDiscriminatorMapProperty($data, ClassMetadata $metadata): string
    {
        return $this->jsonDeserializationVisitor->visitDiscriminatorMapProperty($data, $metadata);
    }

    /**
     * {@inheritdoc}
     */
    public function startVisitingObject(ClassMetadata $metadata, object $data, array $type): void
    {
        $this->jsonDeserializationVisitor->startVisitingObject($metadata, $data, $type);
    }

    /**
     * {@inheritdoc}
     */
    public function visitProperty(PropertyMetadata $metadata, $data)
    {
        return $this->jsonDeserializationVisitor->visitProperty($metadata, $data);
    }

    /**
     * {@inheritdoc}
     */
    public function endVisitingObject(ClassMetadata $metadata, $data, array $type): object
    {
        return $this->jsonDeserializationVisitor->endVisitingObject($metadata, $data, $type);
    }

    /**
     * {@inheritdoc}
     */
    public function getResult($data)
    {
        return $this->jsonDeserializationVisitor->getResult($data);
    }

    /**
     * {@inheritdoc}
     */
    public function prepare($data)
    {
        return $this->jsonDeserializationVisitor->prepare($data);
    }
    
    /**
     * {@inheritdoc}
     */
    public function setNavigator(GraphNavigatorInterface $navigator): void
    {
        $this->jsonDeserializationVisitor->setNavigator($navigator);
    }

}
