<?php
/*
 * This file is part of the PHP_TokenStream package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * A stream of PHP tokens.
 *
 * @author    Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright Sebastian Bergmann <sebastian@phpunit.de>
 * @license   http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version   Release: @package_version@
 * @link      http://github.com/sebastianbergmann/php-token-stream/tree
 * @since     Class available since Release 1.0.0
 */
class PHP_Token_Stream implements ArrayAccess, Countable, SeekableIterator
{
    /**
     * @var array
     */
    protected static $customTokens = array(
      '(' => 'PHP_Token_OPEN_BRACKET',
      ')' => 'PHP_Token_CLOSE_BRACKET',
      '[' => 'PHP_Token_OPEN_SQUARE',
      ']' => 'PHP_Token_CLOSE_SQUARE',
      '{' => 'PHP_Token_OPEN_CURLY',
      '}' => 'PHP_Token_CLOSE_CURLY',
      ';' => 'PHP_Token_SEMICOLON',
      '.' => 'PHP_Token_DOT',
      ',' => 'PHP_Token_COMMA',
      '=' => 'PHP_Token_EQUAL',
      '<' => 'PHP_Token_LT',
      '>' => 'PHP_Token_GT',
      '+' => 'PHP_Token_PLUS',
      '-' => 'PHP_Token_MINUS',
      '*' => 'PHP_Token_MULT',
      '/' => 'PHP_Token_DIV',
      '?' => 'PHP_Token_QUESTION_MARK',
      '!' => 'PHP_Token_EXCLAMATION_MARK',
      ':' => 'PHP_Token_COLON',
      '"' => 'PHP_Token_DOUBLE_QUOTES',
      '@' => 'PHP_Token_AT',
      '&' => 'PHP_Token_AMPERSAND',
      '%' => 'PHP_Token_PERCENT',
      '|' => 'PHP_Token_PIPE',
      '$' => 'PHP_Token_DOLLAR',
      '^' => 'PHP_Token_CARET',
      '~' => 'PHP_Token_TILDE',
      '`' => 'PHP_Token_BACKTICK'
    );

    /**
     * @var string
     */
    protected $filename;

    /**
     * @var array
     */
    protected $tokens = array();

    /**
     * @var integer
     */
    protected $position = 0;

    /**
     * @var array
     */
    protected $linesOfCode = array('loc' => 0, 'cloc' => 0, 'ncloc' => 0);

    /**
     * @var array
     */
    protected $classes;

    /**
     * @var array
     */
    protected $functions;

    /**
     * @var array
     */
    protected $includes;

    /**
     * @var array
     */
    protected $interfaces;

    /**
     * @var array
     */
    protected $traits;

    /**
     * @var array
     */
    protected $lineToFunctionMap = array();

    /**
     * Constructor.
     *
     * @param string $sourceCode
     */
    public function __construct($sourceCode)
    {
        if (is_file($sourceCode)) {
            $this->filename = $sourceCode;
            $sourceCode     = file_get_contents($sourceCode);
        }

        $this->scan($sourceCode);
    }

    /**
     * Destructor.
     */
    public function __destruct()
    {
        $this->tokens = array();
    }

    /**
     * @return string
     */
    public function __toString()
    {
        $buffer = '';

        foreach ($this as $token) {
            $buffer .= $token;
        }

        return $buffer;
    }

    /**
     * @return string
     * @since  Method available since Release 1.1.0
     */
    public function getFilename()
    {
        return $this->filename;
    }

    /**
     * Scans the source for sequences of characters and converts them into a
     * stream of tokens.
     *
     * @param string $sourceCode
     */
    protected function scan($sourceCode)
    {
        $line      = 1;
        $tokens    = token_get_all($sourceCode);
        $numTokens = count($tokens);

        $lastNonWhitespaceTokenWasDoubleColon = FALSE;

        for ($i = 0; $i < $numTokens; ++$i) {
            $token = $tokens[$i];
            unset($tokens[$i]);

            if (is_array($token)) {
                $name = substr(token_name($token[0]), 2);
                $text = $token[1];

                if ($lastNonWhitespaceTokenWasDoubleColon && $name == 'CLASS') {
                    $name = 'CLASS_NAME_CONSTANT';
                }

                $tokenClass = 'PHP_Token_' . $name;
            } else {
                $text       = $token;
                $tokenClass = self::$customTokens[$token];
            }

            $this->tokens[] = new $tokenClass($text, $line, $this, $i);
            $lines          = substr_count($text, "\n");
            $line          += $lines;

            if ($tokenClass == 'PHP_Token_HALT_COMPILER') {
                break;
            }

            else if ($tokenClass == 'PHP_Token_COMMENT' ||
                $tokenClass == 'PHP_Token_DOC_COMMENT') {
                $this->linesOfCode['cloc'] += $lines + 1;
            }

            if ($name == 'DOUBLE_COLON') {
                $lastNonWhitespaceTokenWasDoubleColon = TRUE;
            }

            else if ($name != 'WHITESPACE') {
                $lastNonWhitespaceTokenWasDoubleColon = FALSE;
            }
        }

        $this->linesOfCode['loc']   = substr_count($sourceCode, "\n");
        $this->linesOfCode['ncloc'] = $this->linesOfCode['loc'] -
                                      $this->linesOfCode['cloc'];
    }

    /**
     * @return integer
     */
    public function count()
    {
        return count($this->tokens);
    }

    /**
     * @return PHP_Token[]
     */
    public function tokens()
    {
        return $this->tokens;
    }

    /**
     * @return array
     */
    public function getClasses()
    {
        if ($this->classes !== NULL) {
            return $this->classes;
        }

        $this->parse();

        return $this->classes;
    }

    /**
     * @return array
     */
    public function getFunctions()
    {
        if ($this->functions !== NULL) {
            return $this->functions;
        }

        $this->parse();

        return $this->functions;
    }

    /**
     * @return array
     */
    public function getInterfaces()
    {
        if ($this->interfaces !== NULL) {
            return $this->interfaces;
        }

        $this->parse();

        return $this->interfaces;
    }

    /**
     * @return array
     * @since  Method available since Release 1.1.0
     */
    public function getTraits()
    {
        if ($this->traits !== NULL) {
            return $this->traits;
        }

        $this->parse();

        return $this->traits;
    }

    /**
     * Gets the names of all files that have been included
     * using include(), include_once(), require() or require_once().
     *
     * Parameter $categorize set to TRUE causing this function to return a
     * multi-dimensional array with categories in the keys of the first dimension
     * and constants and their values in the second dimension.
     *
     * Parameter $category allow to filter following specific inclusion type
     *
     * @param bool   $categorize OPTIONAL
     * @param string $category   OPTIONAL Either 'require_once', 'require',
     *                                           'include_once', 'include'.
     * @return array
     * @since  Method available since Release 1.1.0
     */
    public function getIncludes($categorize = FALSE, $category = NULL)
    {
        if ($this->includes === NULL) {
            $this->includes = array(
              'require_once' => array(),
              'require'      => array(),
              'include_once' => array(),
              'include'      => array()
            );

            foreach ($this->tokens as $token) {
                switch (get_class($token)) {
                    case 'PHP_Token_REQUIRE_ONCE':
                    case 'PHP_Token_REQUIRE':
                    case 'PHP_Token_INCLUDE_ONCE':
                    case 'PHP_Token_INCLUDE': {
                        $this->includes[$token->getType()][] = $token->getName();
                    }
                    break;
                }
            }
        }

        if (isset($this->includes[$category])) {
            $includes = $this->includes[$category];
        }

        else if ($categorize === FALSE) {
            $includes = array_merge(
              $this->includes['require_once'],
              $this->includes['require'],
              $this->includes['include_once'],
              $this->includes['include']
            );
        } else {
            $includes = $this->includes;
        }

        return $includes;
    }

    /**
     * Returns the name of the function or method a line belongs to.
     *
     * @return string or null if the line is not in a function or method
     * @since  Method available since Release 1.2.0
     */
    public function getFunctionForLine($line)
    {
        $this->parse();

        if (isset($this->lineToFunctionMap[$line])) {
            return $this->lineToFunctionMap[$line];
        }
    }

    protected function parse()
    {
        $this->interfaces = array();
        $this->classes    = array();
        $this->traits     = array();
        $this->functions  = array();
        $class            = FALSE;
        $classEndLine     = FALSE;
        $trait            = FALSE;
        $traitEndLine     = FALSE;
        $interface        = FALSE;
        $interfaceEndLine = FALSE;

        foreach ($this->tokens as $token) {
            switch (get_class($token)) {
                case 'PHP_Token_HALT_COMPILER': {
                    return;
                }
                break;

                case 'PHP_Token_INTERFACE': {
                    $interface        = $token->getName();
                    $interfaceEndLine = $token->getEndLine();

                    $this->interfaces[$interface] = array(
                      'methods'   => array(),
                      'parent'    => $token->getParent(),
                      'keywords'  => $token->getKeywords(),
                      'docblock'  => $token->getDocblock(),
                      'startLine' => $token->getLine(),
                      'endLine'   => $interfaceEndLine,
                      'package'   => $token->getPackage(),
                      'file'      => $this->filename
                    );
                }
                break;

                case 'PHP_Token_CLASS':
                case 'PHP_Token_TRAIT': {
                    $tmp = array(
                      'methods'   => array(),
                      'parent'    => $token->getParent(),
                      'interfaces'=> $token->getInterfaces(),
                      'keywords'  => $token->getKeywords(),
                      'docblock'  => $token->getDocblock(),
                      'startLine' => $token->getLine(),
                      'endLine'   => $token->getEndLine(),
                      'package'   => $token->getPackage(),
                      'file'      => $this->filename
                    );

                    if ($token instanceof PHP_Token_CLASS) {
                        $class                 = $token->getName();
                        $classEndLine          = $token->getEndLine();
                        $this->classes[$class] = $tmp;
                    } else {
                        $trait                = $token->getName();
                        $traitEndLine         = $token->getEndLine();
                        $this->traits[$trait] = $tmp;
                    }
                }
                break;

                case 'PHP_Token_FUNCTION': {
                    $name = $token->getName();
                    $tmp  = array(
                      'docblock'  => $token->getDocblock(),
                      'keywords'  => $token->getKeywords(),
                      'visibility'=> $token->getVisibility(),
                      'signature' => $token->getSignature(),
                      'startLine' => $token->getLine(),
                      'endLine'   => $token->getEndLine(),
                      'ccn'       => $token->getCCN(),
                      'file'      => $this->filename
                    );

                    if ($class === FALSE &&
                        $trait === FALSE &&
                        $interface === FALSE) {
                        $this->functions[$name] = $tmp;

                        $this->addFunctionToMap(
                          $name, $tmp['startLine'], $tmp['endLine']
                        );
                    }

                    else if ($class !== FALSE) {
                        $this->classes[$class]['methods'][$name] = $tmp;

                        $this->addFunctionToMap(
                          $class . '::' . $name,
                          $tmp['startLine'],
                          $tmp['endLine']
                        );
                    }

                    else if ($trait !== FALSE) {
                        $this->traits[$trait]['methods'][$name] = $tmp;

                        $this->addFunctionToMap(
                          $trait . '::' . $name,
                          $tmp['startLine'],
                          $tmp['endLine']
                        );
                    }

                    else {
                        $this->interfaces[$interface]['methods'][$name] = $tmp;
                    }
                }
                break;

                case 'PHP_Token_CLOSE_CURLY': {
                    if ($classEndLine !== FALSE &&
                        $classEndLine == $token->getLine()) {
                        $class        = FALSE;
                        $classEndLine = FALSE;
                    }

                    else if ($traitEndLine !== FALSE &&
                        $traitEndLine == $token->getLine()) {
                        $trait        = FALSE;
                        $traitEndLine = FALSE;
                    }

                    else if ($interfaceEndLine !== FALSE &&
                        $interfaceEndLine == $token->getLine()) {
                        $interface        = FALSE;
                        $interfaceEndLine = FALSE;
                    }
                }
                break;
            }
        }
    }

    /**
     * @return array
     */
    public function getLinesOfCode()
    {
        return $this->linesOfCode;
    }

    /**
     */
    public function rewind()
    {
        $this->position = 0;
    }

    /**
     * @return boolean
     */
    public function valid()
    {
        return isset($this->tokens[$this->position]);
    }

    /**
     * @return integer
     */
    public function key()
    {
        return $this->position;
    }

    /**
     * @return PHP_Token
     */
    public function current()
    {
        return $this->tokens[$this->position];
    }

    /**
     */
    public function next()
    {
        $this->position++;
    }

    /**
     * @param mixed $offset
     */
    public function offsetExists($offset)
    {
        return isset($this->tokens[$offset]);
    }

    /**
     * @param  mixed $offset
     * @return mixed
     */
    public function offsetGet($offset)
    {
        return $this->tokens[$offset];
    }

    /**
     * @param mixed $offset
     * @param mixed $value
     */
    public function offsetSet($offset, $value)
    {
        $this->tokens[$offset] = $value;
    }

    /**
     * @param mixed $offset
     */
    public function offsetUnset($offset)
    {
        unset($this->tokens[$offset]);
    }

    /**
     * Seek to an absolute position.
     *
     * @param  integer $position
     * @throws OutOfBoundsException
     */
    public function seek($position)
    {
        $this->position = $position;

        if (!$this->valid()) {
            throw new OutOfBoundsException('Invalid seek position');
        }
    }

    private function addFunctionToMap($name, $startLine, $endLine)
    {
        for ($line = $startLine; $line <= $endLine; $line++) {
            $this->lineToFunctionMap[$line] = $name;
        }
    }
}
