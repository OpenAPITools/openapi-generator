<?php
/*
 * This file is part of the Version package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace SebastianBergmann;

/**
 * @package   Version
 * @author    Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright Sebastian Bergmann <sebastian@phpunit.de>
 * @license   http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link      http://github.com/sebastianbergmann/version
 * @since     Class available since Release 1.0.0
 */
class Version
{
    private $path;
    private $release;
    private $version;

    /**
     * @param string $release
     * @param string $path
     */
    public function __construct($release, $path)
    {
        $this->release = $release;
        $this->path    = $path;
    }

    /**
     * @return string
     */
    public function getVersion()
    {
        if ($this->version === null) {
            if (count(explode('.', $this->release)) == 3) {
                $this->version = $this->release;
            } else {
                $this->version = $this->release . '-dev';
            }

            $git = $this->getGitInformation($this->path);

            if ($git) {
                if (count(explode('.', $this->release)) == 3) {
                    $this->version = $git;
                } else {
                    $git = explode('-', $git);

                    $this->version = $this->release . '-' . end($git);
                }
            }
        }

        return $this->version;
    }

    /**
     * @param  string $path
     * @return boolean|string
     */
    private function getGitInformation($path)
    {
        if (!is_dir($path . DIRECTORY_SEPARATOR . '.git')) {
            return false;
        }

        $dir = getcwd();
        chdir($path);
        $returnCode = 1;
        $result = @exec('git describe --tags 2>&1', $output, $returnCode);
        chdir($dir);

        if ($returnCode !== 0) {
            return false;
        }

        return $result;
    }
}
