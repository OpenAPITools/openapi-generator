<?php

passthru("php artisan --env='testing' migrate:fresh");
passthru("php artisan --env='testing' db:seed");

require realpath(__DIR__ . '/../vendor/autoload.php');
