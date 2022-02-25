<?php
declare(strict_types=1);

namespace App;

use Articus\DataTransfer as DT;
use Interop\Container\ContainerInterface;
use OpenAPIGenerator\APIClient as OAGAC;

class ApiClientFactory extends DT\ConfigAwareFactory
{
    public function __construct(string $configKey = ApiClient::class)
    {
        parent::__construct($configKey);
    }

    public function __invoke(ContainerInterface $container, $requestedName, array $options = null)
    {
        $config = new OAGAC\ApiClientOptions(\array_merge($this->getServiceConfig($container), $options ?? []));
        return new ApiClient(
            $config->serverUrl,
            $container->get($config->dataTransferServiceName),
            $container->get($config->requestFactoryServiceName),
            $container->get($config->httpClientServiceName),
            $container->get($config->securityProviderFactoryServiceName),
            $container->get($config->bodyCoderFactoryServiceName),
            $container->get($config->bodyCoderFactoryServiceName)
        );
    }
}
