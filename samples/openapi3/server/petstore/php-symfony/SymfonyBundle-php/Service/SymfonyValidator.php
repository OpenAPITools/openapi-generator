<?php

namespace OpenAPI\Server\Service;

use Symfony\Component\Validator\Validator\ValidatorInterface as SymfonyValidatorInterface;

class SymfonyValidator implements ValidatorInterface
{
    protected $validator;

    public function __construct(SymfonyValidatorInterface $validator)
    {
        $this->validator = $validator;
    }

    public function validate($value, $constraints = null, $groups = null)
    {
        return $this->validator->validate($value, $constraints, $groups);
    }
}
