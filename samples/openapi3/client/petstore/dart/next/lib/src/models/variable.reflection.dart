// Model reflection

part of 'variable.dart';


//class reflection

class VariableReflection extends ClassReflection<Variable> {
  static const instance = VariableReflection._(
    name: PropertyReflection(
      dartName: r'name',
      nullable: false,
      required: true,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
    ),
    value: PropertyReflection(
      dartName: r'value',
      nullable: false,
      required: true,
      oasName: r'value',
      oasType: r'Value',
      pattern: null,
    ),
  );
  const VariableReflection._({
    required this.name,
  
    required this.value,
  });

  final PropertyReflection<
            String
> name;
  final PropertyReflection<
            Value
> value;

  @override
  List<PropertyReflection> get members => [
    name,
value,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => Variable.canDeserialize(src);
  @override
  Variable Function(Object? src) get deserializeFunction =>
      (src) => Variable.deserialize(src);

  @override
  Object? Function(Variable src) get serializeFunction =>
      (src) => src.serialize();
}

class VariableXmlReflection {
    const VariableXmlReflection();
}

