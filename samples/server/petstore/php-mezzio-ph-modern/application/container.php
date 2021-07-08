<?php
declare(strict_types=1);

use Laminas\Config\Factory as ConfigFactory;

//Use Composer autoload that includes code both from ../src and ../vendor
require __DIR__ . '/../vendor/autoload.php';

//Path to file for caching full configuration
const CONFIG_CACHE_PATH = __DIR__ . '/../data/cache/config.php';

//Get full configuration
$config = [];
if (is_readable(CONFIG_CACHE_PATH)) {
    $config = include CONFIG_CACHE_PATH;
} else {
    //Register extra extension for YAML files
    ConfigFactory::registerReader('yml', 'yaml');

    //Combine all configuration files in right order
    $config = ConfigFactory::fromFiles([
        __DIR__ . '/config/data_transfer.yml',
        __DIR__ . '/config/path_handler.yml',
        __DIR__ . '/config/app.yml',
        __DIR__ . '/config.yml',
    ]);

    //Cache full configuration
    if ($config['cache_configuration'] ?? false) {
        if (!ConfigFactory::toFile(CONFIG_CACHE_PATH, $config)) {
            throw new \RuntimeException('Failed to cache configuration');
        }
    }
}

//Create container
$container = new \Laminas\ServiceManager\ServiceManager($config['dependencies'] ?? []);

//Register full configuration as a service
$container->setService('config', $config);
$container->setAlias('Config', 'config');

return $container;
