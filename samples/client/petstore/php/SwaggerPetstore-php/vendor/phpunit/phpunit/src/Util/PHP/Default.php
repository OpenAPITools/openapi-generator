<?php
/*
 * This file is part of PHPUnit.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

use SebastianBergmann\Environment\Runtime;

/**
 * Default utility for PHP sub-processes.
 *
 * @package    PHPUnit
 * @subpackage Util
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.5.12
 */
class PHPUnit_Util_PHP_Default extends PHPUnit_Util_PHP
{
    /**
     * Runs a single job (PHP code) using a separate PHP process.
     *
     * @param  string                      $job
     * @param  array                       $settings
     * @return array
     * @throws PHPUnit_Framework_Exception
     */
    public function runJob($job, array $settings = array())
    {
        $runtime = new Runtime;

        $process = proc_open(
            $runtime->getBinary() . $this->settingsToParameters($settings),
            array(
            0 => array('pipe', 'r'),
            1 => array('pipe', 'w'),
            2 => array('pipe', 'w')
            ),
            $pipes
        );

        if (!is_resource($process)) {
            throw new PHPUnit_Framework_Exception(
                'Unable to spawn worker process'
            );
        }

        $this->process($pipes[0], $job);
        fclose($pipes[0]);

        $stdout = stream_get_contents($pipes[1]);
        fclose($pipes[1]);

        $stderr = stream_get_contents($pipes[2]);
        fclose($pipes[2]);

        proc_close($process);
        $this->cleanup();

        return array('stdout' => $stdout, 'stderr' => $stderr);
    }

    /**
     * @param  resource                    $pipe
     * @param  string                      $job
     * @throws PHPUnit_Framework_Exception
     * @since Method available since Release 3.5.12
     */
    protected function process($pipe, $job)
    {
        fwrite($pipe, $job);
    }

    /**
     * @since Method available since Release 3.5.12
     */
    protected function cleanup()
    {
    }
}
