<?php
/**
 * Capitalization
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * Capitalization
 */
class Capitalization
{

    /** @var string $smallCamel (optional) */
    private $smallCamel;

    /** @var string $capitalCamel (optional) */
    private $capitalCamel;

    /** @var string $smallSnake (optional) */
    private $smallSnake;

    /** @var string $capitalSnake (optional) */
    private $capitalSnake;

    /** @var string $sCAETHFlowPoints (optional) */
    private $sCAETHFlowPoints;

    /** @var string $aTTNAME (optional) Name of the pet */
    private $aTTNAME;

    /**
     * Capitalization constructor
     *
     * @param string|null $smallCamel (optional)
     * @param string|null $capitalCamel (optional)
     * @param string|null $smallSnake (optional)
     * @param string|null $capitalSnake (optional)
     * @param string|null $sCAETHFlowPoints (optional)
     * @param string|null $aTTNAME (optional) Name of the pet
     */
    public function __construct(
        $smallCamel = null,
        $capitalCamel = null,
        $smallSnake = null,
        $capitalSnake = null,
        $sCAETHFlowPoints = null,
        $aTTNAME = null
    ) {
        $this->smallCamel = $smallCamel;
        $this->capitalCamel = $capitalCamel;
        $this->smallSnake = $smallSnake;
        $this->capitalSnake = $capitalSnake;
        $this->sCAETHFlowPoints = $sCAETHFlowPoints;
        $this->aTTNAME = $aTTNAME;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $capitalization = Capitalization::createFromObject([ 'smallCamel' => 'foobar' ]);
     *
     * @return Capitalization
     */
    public static function createFromObject(array $data = null)
    {
        $smallCamel = (isset($data['smallCamel'])) ? $data['smallCamel'] : null;
        $capitalCamel = (isset($data['CapitalCamel'])) ? $data['CapitalCamel'] : null;
        $smallSnake = (isset($data['small_Snake'])) ? $data['small_Snake'] : null;
        $capitalSnake = (isset($data['Capital_Snake'])) ? $data['Capital_Snake'] : null;
        $sCAETHFlowPoints = (isset($data['SCA_ETH_Flow_Points'])) ? $data['SCA_ETH_Flow_Points'] : null;
        $aTTNAME = (isset($data['ATT_NAME'])) ? $data['ATT_NAME'] : null;
        return new Capitalization(
            $smallCamel,
            $capitalCamel,
            $smallSnake,
            $capitalSnake,
            $sCAETHFlowPoints,
            $aTTNAME
        );
    }
}
