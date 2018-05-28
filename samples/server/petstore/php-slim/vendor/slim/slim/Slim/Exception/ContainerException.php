<?php
/**
 * Slim Framework (http://slimframework.com)
 *
 * @link      https://github.com/codeguy/Slim
 * @copyright Copyright (c) 2011-2015 Josh Lockhart
 * @license   https://github.com/codeguy/Slim/blob/master/LICENSE (MIT License)
 */
namespace Slim\Exception;

use InvalidArgumentException;
use Interop\Container\Exception\ContainerException as InteropContainerException;

/**
 * Container Exception
 */
class ContainerException extends InvalidArgumentException implements InteropContainerException
{

}
