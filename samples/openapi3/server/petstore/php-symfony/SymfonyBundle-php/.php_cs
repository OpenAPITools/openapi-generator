<?php

return Symfony\CS\Config::create()
    ->level(Symfony\CS\FixerInterface::PSR2_LEVEL)
    ->setUsingCache(true)
    ->fixers(
        [
            'ordered_use',
            'phpdoc_order',
            'short_array_syntax',
            'strict',
            'strict_param'
        ]
    )
    ->finder(
        Symfony\CS\Finder\DefaultFinder::create()
            ->in(__DIR__)
    );
