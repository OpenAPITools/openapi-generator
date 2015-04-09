#!/usr/bin/env php
<?php
/**
 * Build a PHPCS phar.
 *
 * PHP version 5
 *
 * @category  PHP
 * @package   PHP_CodeSniffer
 * @author    Benjamin Pearson <bpearson@squiz.com.au>
 * @author    Greg Sherwood <gsherwood@squiz.net>
 * @copyright 2006-2014 Squiz Pty Ltd (ABN 77 084 670 600)
 * @license   https://github.com/squizlabs/PHP_CodeSniffer/blob/master/licence.txt BSD Licence
 * @link      http://pear.php.net/package/PHP_CodeSniffer
 */

error_reporting(E_ALL | E_STRICT);

if (ini_get('phar.readonly') === '1') {
    echo 'Unable to build, phar.readonly in php.ini is set to read only.'.PHP_EOL;
    exit(1);
}

$cwd = getCwd();
require_once __DIR__.'/../CodeSniffer.php';

$scripts = array(
            'phpcs',
            'phpcbf',
           );

foreach ($scripts as $script) {
    echo "Building $script phar".PHP_EOL;

    $pharFile = $cwd.'/'.$script.'.phar';
    echo "\t=> $pharFile".PHP_EOL;
    if (file_exists($pharFile) === true) {
        echo "\t** file exists, removing **".PHP_EOL;
        unlink($pharFile);
    }

    $phar = new Phar($pharFile, 0, $script.'.phar');

    echo "\t=> adding files from package.xml... ";
    buildFromPackage($phar);
    echo 'done'.PHP_EOL;

    echo "\t=> adding stub... ";
    $stub  = '#!/usr/bin/env php'."\n";
    $stub .= '<?php'."\n";
    $stub .= 'Phar::mapPhar(\''.$script.'.phar\');'."\n";
    $stub .= 'require_once "phar://'.$script.'.phar/CodeSniffer/CLI.php";'."\n";
    $stub .= '$cli = new PHP_CodeSniffer_CLI();'."\n";
    $stub .= '$cli->run'.$script.'();'."\n";
    $stub .= '__HALT_COMPILER();';
    $phar->setStub($stub);
    echo 'done'.PHP_EOL;
}//end foreach


/**
 * Build from a package list.
 *
 * @param object $phar The Phar class.
 *
 * @return void
 */
function buildFromPackage(&$phar)
{
    $packageFile = realpath(__DIR__.'/../package.xml');
    $dom         = new DOMDocument('1.0', 'utf-8');
    $loaded      = $dom->loadXML(file_get_contents($packageFile));
    if ($loaded === false) {
        echo "Unable to load package file: $packageFile".PHP_EOL;
        exit(1);
    }

    $contents  = $dom->getElementsByTagName('contents');
    $topLevels = $contents->item(0)->childNodes;
    $tlLength  = $topLevels->length;
    for ($l = 0; $l < $tlLength; $l++) {
        $currentLevel = $topLevels->item($l);
        buildFromNode($phar, $currentLevel, '');
    }

    // Add licence file.
    $phar->addFile(realpath(__DIR__.'/../licence.txt'), 'licence.txt');
    $phar['licence.txt']->compress(Phar::GZ);

}//end buildFromPackage()


/**
 * Add from a node.
 *
 * @param object $phar   The Phar class.
 * @param object $node   The node to add.
 * @param string $prefix The prefix of the structure.
 *
 * @return void
 */
function buildFromNode(&$phar, $node, $prefix='')
{
    $nodeName = $node->nodeName;
    if ($nodeName !== 'dir' && $nodeName !== 'file') {
        // Invalid node.
        return;
    }

    $path = $prefix.$node->getAttribute('name');
    if ($node->getAttribute('role') === 'php' || $node->getAttribute('role') === 'data') {
        $path = ltrim($path, '/');
        $phar->addFile(realpath(__DIR__.'/../'.$path), $path);
        $phar[$path]->compress(Phar::GZ);
    }

    if ($nodeName === 'dir') {
        // Descend into the depths.
        $path     = rtrim($path, '/').'/';
        $children = $node->childNodes;
        $childLn  = $children->length;
        for ($c = 0; $c < $childLn; $c++) {
            $child = $children->item($c);
            buildFromNode($phar, $child, $path);
        }
    }

}//end buildFromNode()
