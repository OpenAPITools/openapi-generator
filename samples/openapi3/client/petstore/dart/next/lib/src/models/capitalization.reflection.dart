// Model reflection

part of 'capitalization.dart';


//class reflection

class CapitalizationReflection extends ClassReflection<Capitalization> {
  static const instance = CapitalizationReflection._(
    smallCamel: PropertyReflection(
      dartName: r'smallCamel',
      nullable: false,
      required: false,
      oasName: r'smallCamel',
      oasType: r'string',
      pattern: null,
    ),
    capitalCamel: PropertyReflection(
      dartName: r'capitalCamel',
      nullable: false,
      required: false,
      oasName: r'CapitalCamel',
      oasType: r'string',
      pattern: null,
    ),
    smallSnake: PropertyReflection(
      dartName: r'smallSnake',
      nullable: false,
      required: false,
      oasName: r'small_Snake',
      oasType: r'string',
      pattern: null,
    ),
    capitalSnake: PropertyReflection(
      dartName: r'capitalSnake',
      nullable: false,
      required: false,
      oasName: r'Capital_Snake',
      oasType: r'string',
      pattern: null,
    ),
    scAETHFlowPoints: PropertyReflection(
      dartName: r'scAETHFlowPoints',
      nullable: false,
      required: false,
      oasName: r'SCA_ETH_Flow_Points',
      oasType: r'string',
      pattern: null,
    ),
    ATT_NAME: PropertyReflection(
      dartName: r'ATT_NAME',
      nullable: false,
      required: false,
      oasName: r'ATT_NAME',
      oasType: r'string',
      pattern: null,
    ),
  );
  const CapitalizationReflection._({
    required this.smallCamel,
  
    required this.capitalCamel,
  
    required this.smallSnake,
  
    required this.capitalSnake,
  
    required this.scAETHFlowPoints,
  
    required this.ATT_NAME,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> smallCamel;
  final PropertyReflection<UndefinedWrapper<
            String
>> capitalCamel;
  final PropertyReflection<UndefinedWrapper<
            String
>> smallSnake;
  final PropertyReflection<UndefinedWrapper<
            String
>> capitalSnake;
  final PropertyReflection<UndefinedWrapper<
            String
>> scAETHFlowPoints;
  final PropertyReflection<UndefinedWrapper<
            String
>> ATT_NAME;

  @override
  List<PropertyReflection> get members => [
    smallCamel,
capitalCamel,
smallSnake,
capitalSnake,
scAETHFlowPoints,
ATT_NAME,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Capitalization.canDeserialize(src);
  @override
  Capitalization Function(Object? src) get deserializeFunction =>
      (src) => Capitalization.deserialize(src);

  @override
  Object? Function(Capitalization src) get serializeFunction =>
      (src) => src.serialize();
}

class CapitalizationXmlReflection {
    const CapitalizationXmlReflection();
}

