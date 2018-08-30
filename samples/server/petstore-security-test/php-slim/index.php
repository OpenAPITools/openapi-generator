<?php
/**
 * OpenAPI Petstore *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r
 * @version 1.0.0 *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r
 */

require_once __DIR__ . '/vendor/autoload.php';

use OpenAPIServer\SlimRouter;

$router = new SlimRouter();
$app = $router->getSlimApp();
$app->run();
