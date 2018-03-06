<?php
use Zend\Config\Factory as ConfigFactory;

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
    if (isset($config['cache_configuration']) && $config['cache_configuration']) {
        if (!ConfigFactory::toFile(CONFIG_CACHE_PATH, $config)) {
            throw new \RuntimeException('Failed to cache configuration');
        }
    }
}
//Get configuration for container
$dependencies = [];
if (isset($config['dependencies'])) {
    $dependencies = $config['dependencies'];
}

//Create container
$container = new \Zend\ServiceManager\ServiceManager($dependencies);

//Register full configuration as a service
$container->setService('config', $config);
$container->setAlias('Config', 'config');

return $container;
