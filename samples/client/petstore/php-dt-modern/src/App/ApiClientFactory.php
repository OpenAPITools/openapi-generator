<?php
declare(strict_types=1);

namespace App;

use Articus\PluginManager as PM;
use OpenAPIGenerator\APIClient as OAGAC;
use Psr\Container\ContainerInterface;

class ApiClientFactory implements PM\ServiceFactoryInterface
{
    use PM\ConfigAwareFactoryTrait;

    public function __construct(string $configKey = ApiClient::class)
    {
        $this->configKey = $configKey;
    }

    public function __invoke(ContainerInterface $container, $requestedName, ?array $options = null): ApiClient
    {
        $config = new OAGAC\ApiClientOptions(\array_merge($this->getServiceConfig($container), $options ?? []));
        return new ApiClient(
            $config->serverUrl,
            $container->get($config->dataTransferServiceName),
            $container->get($config->requestFactoryServiceName),
            $container->get($config->httpClientServiceName),
            $container->get($config->securityProviderFactoryServiceName),
            $container->get($config->bodyCoderFactoryServiceName),
            $container->get($config->bodyCoderFactoryServiceName),
            $container->get($config->contentStrategyFactoryServiceName),
            $container->get($config->contentValidatorFactoryServiceName)
        );
    }
}
