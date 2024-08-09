// Model reflection

part of 'update_pet_with_form_request.dart';


//class reflection

class UpdatePetWithFormRequestReflection extends ClassReflection<UpdatePetWithFormRequest> {
  static UpdatePetWithFormRequestReflection instanceGetter() => instance;
  static const instance = UpdatePetWithFormRequestReflection._(
    modelName: r'updatePetWithForm_request',
    className: r'UpdatePetWithFormRequest',
    namePart: PropertyReflection<UpdatePetWithFormRequest, UndefinedWrapper<
            String

>>(
      dartName: r'name',
      nullable: false,
      required: false,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _nameGetter,
      setter: _nameSetter,
    ),
    statusPart: PropertyReflection<UpdatePetWithFormRequest, UndefinedWrapper<
            String

>>(
      dartName: r'status',
      nullable: false,
      required: false,
      oasName: r'status',
      oasType: r'string',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _statusGetter,
      setter: _statusSetter,
    ),
    
    
    additionalPropertiesPart: AdditionalPropertiesReflection(
      parentReflectionGetter: instanceGetter,
      itemsReflection: ItemsReflection<UpdatePetWithFormRequest, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const UpdatePetWithFormRequestReflection._({
    required this.modelName,
    required this.className,
    required this.namePart,
    required this.statusPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<UpdatePetWithFormRequest, UndefinedWrapper<
            String

>> namePart;
  static UndefinedWrapper<
            String

> _nameGetter(UpdatePetWithFormRequest parent) {
    return parent.name;
  }
  static void _nameSetter(UpdatePetWithFormRequest parent, UndefinedWrapper<
            String

> value) {
    parent.name = value;
  }
  final PropertyReflection<UpdatePetWithFormRequest, UndefinedWrapper<
            String

>> statusPart;
  static UndefinedWrapper<
            String

> _statusGetter(UpdatePetWithFormRequest parent) {
    return parent.status;
  }
  static void _statusSetter(UpdatePetWithFormRequest parent, UndefinedWrapper<
            String

> value) {
    parent.status = value;
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
  List<PropertyReflection<UpdatePetWithFormRequest, dynamic>> get properties => [
    namePart,
statusPart,
  ];

  final AdditionalPropertiesReflection<UpdatePetWithFormRequest, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<UpdatePetWithFormRequest, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<UpdatePetWithFormRequest, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => UpdatePetWithFormRequest.canDeserialize(src);
  @override
  UpdatePetWithFormRequest Function(Object? src) get deserializeFunction =>
      (src) => UpdatePetWithFormRequest.deserialize(src);

  @override
  Object? Function(UpdatePetWithFormRequest src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of UpdatePetWithFormRequest.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  UpdatePetWithFormRequest example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
        discriminatorExampleResults = const {},}) {
    final _reflection = this;
    final actualDiscriminators = discriminators ?? _reflection.aggregatedDiscriminators;
    discriminatorExampleResults = Map.from(discriminatorExampleResults);
    for (final MapEntry(key: propName, value: mappings) in actualDiscriminators.entries) {
      if (discriminatorExampleResults.containsKey(propName)) {
        continue;
      }
      final r =  exampleDiscriminator(mappings);
      if (r != null){
        discriminatorExampleResults[propName] = r;
      }
    }

    final exampleResult = UpdatePetWithFormRequest(
      name: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[namePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      status: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[statusPart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return UndefinedWrapper(result);
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class UpdatePetWithFormRequestXmlReflection {
    const UpdatePetWithFormRequestXmlReflection();
}

