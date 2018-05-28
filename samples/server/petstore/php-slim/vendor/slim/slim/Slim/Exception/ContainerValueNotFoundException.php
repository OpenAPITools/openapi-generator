<?php
/**
 * Slim Framework (http://slimframework.com)
 *
 * @link      https://github.com/codeguy/Slim
 * @copyright Copyright (c) 2011-2016 Josh Lockhart
 * @license   https://github.com/codeguy/Slim/blob/master/LICENSE (MIT License)
 */
namespace Slim\Exception;

use RuntimeException;
use Interop\Container\Exception\NotFoundException as InteropNotFoundException;

/**
 * Not Found Exception
 */
class ContainerValueNotFoundException extends RuntimeException implements InteropNotFoundException
{

}
