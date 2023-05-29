<?php

namespace OpenAPI\Server\Tests;

use Symfony\Bundle\FrameworkBundle\FrameworkBundle;
use Symfony\Component\HttpKernel\Kernel;
use Symfony\Component\Config\Loader\LoaderInterface;

class AppKernel extends Kernel
{
    /**
     * @inheritDoc
     */
    public function registerBundles(): iterable
    {
        return [
            new FrameworkBundle(),
        ];
    }

    /**
     * @inheritDoc
     * @throws \Exception
     */
    public function registerContainerConfiguration(LoaderInterface $loader)
    {
        $loader->load(__DIR__.'/test_config.yaml');
    }
}
