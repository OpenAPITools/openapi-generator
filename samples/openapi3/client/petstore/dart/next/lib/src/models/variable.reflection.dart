// Model reflection

part of 'variable.dart';


//class reflection

class VariableReflection extends ClassReflection<Variable> {
  static VariableReflection instanceGetter() => instance;
  static const instance = VariableReflection._(
    modelName: r'Variable',
    className: r'Variable',
    namePart: PropertyReflection<Variable, 
            String
>(
      dartName: r'name',
      nullable: false,
      required: true,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _nameGetter,
      setter: _nameSetter,
    ),
    valuePart: PropertyReflection<Variable, 
            Value
>(
      dartName: r'value',
      nullable: false,
      required: true,
      oasName: r'value',
      oasType: r'Value',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      classReflection: ValueReflection.instance,
      getter: _valueGetter,
      setter: _valueSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<Variable, Object
?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const VariableReflection._({
    required this.modelName,
    required this.className,
    required this.namePart,
    required this.valuePart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<Variable, 
            String
> namePart;
  static 
            String
 _nameGetter(Variable parent) {
    return parent.name;
  }
  static void _nameSetter(Variable parent, 
            String
 value) {
    parent.name = value;
  }
  final PropertyReflection<Variable, 
            Value
> valuePart;
  static 
            Value
 _valueGetter(Variable parent) {
    return parent.value;
  }
  static void _valueSetter(Variable parent, 
            Value
 value) {
    parent.value = value;
  }



  @override
  final Map<String, ClassReflection> discriminatorMappings;
  @override
  final Map<String, ClassReflection> discriminatorImplicitMappings;
  @override
  final String? discriminatorKey;
  @override
  final String modelName;
  @override
  final String className;


  @override
  List<PropertyReflection<Variable, dynamic>> get properties => [
    namePart,
valuePart,
  ];

  final AdditionalPropertiesReflection<Variable, Object
?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<Variable, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<Variable, dynamic>> get allOfs => [
    
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

  /// Gets an example of Variable.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  Variable example({Map<String, (ClassReflection, PropertyReflection)> discriminators = const {}}) {
    final _reflection = this;
    if (discriminators.isEmpty) discriminators = _reflection.aggregatedDiscriminators;
    return Variable(
      name: () {
        PartReflection? _partReflection = _reflection.namePart;
        
        final disc = discriminators[r'name'];
        if (disc != null) {
          final result = exampleDiscriminator(_partReflection, disc);
          if (result != null) {
            return result;
          }
        }
        
        return 


            
            


    
    exampleString()


;
      }(),
      value: () {
        PartReflection? _partReflection = _reflection.valuePart;
        
        return 


            
            


    Value.$reflection.example()
    


;
      }(),
      additionalProperties: () { PartReflection? _partReflection = _reflection.additionalPropertiesPart; return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
      
    );
  }
}

class VariableXmlReflection {
    const VariableXmlReflection();
}

