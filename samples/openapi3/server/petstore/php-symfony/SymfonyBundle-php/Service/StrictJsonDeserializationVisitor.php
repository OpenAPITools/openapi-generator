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

namespace OpenAPI\Server\Service;

use JMS\Serializer\Context;
use JMS\Serializer\JsonDeserializationVisitor;

class StrictJsonDeserializationVisitor extends JsonDeserializationVisitor
{
    /**
     * {@inheritdoc}
     */
    public function visitString($data, array $type, Context $context)
    {
        if (!is_string($data)) {
            throw TypeMismatchException::fromValue('string', $data, $context);
        }

        return parent::visitString($data, $type, $context);
    }

    /**
     * {@inheritdoc}
     */
    public function visitBoolean($data, array $type, Context $context)
    {
        if (!is_bool($data)) {
            throw TypeMismatchException::fromValue('boolean', $data, $context);
        }

        return parent::visitBoolean($data, $type, $context);
    }

    /**
     * {@inheritdoc}
     */
    public function visitInteger($data, array $type, Context $context)
    {
        if (!is_int($data)) {
            throw TypeMismatchException::fromValue('integer', $data, $context);
        }

        return parent::visitInteger($data, $type, $context);
    }

    /**
     * {@inheritdoc}
     */
    public function visitDouble($data, array $type, Context $context)
    {
        if (!is_float($data) && !is_integer($data)) {
            throw TypeMismatchException::fromValue('double', $data, $context);
        }

        return parent::visitDouble($data, $type, $context);
    }
}
