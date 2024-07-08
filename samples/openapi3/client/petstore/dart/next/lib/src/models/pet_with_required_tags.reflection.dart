// Model reflection

part of 'pet_with_required_tags.dart';


//class reflection

class PetWithRequiredTagsReflection extends ClassReflection<PetWithRequiredTags> {
  static PetWithRequiredTagsReflection instanceGetter() => instance;
  static const instance = PetWithRequiredTagsReflection._(
    modelName: r'PetWithRequiredTags',
    className: r'PetWithRequiredTags',
    idPart: PropertyReflection<PetWithRequiredTags, UndefinedWrapper<
            int

>>(
      dartName: r'id',
      nullable: false,
      required: false,
      oasName: r'id',
      oasType: r'integer',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      getter: _idGetter,
      setter: _idSetter,
    ),
    categoryPart: PropertyReflection<PetWithRequiredTags, UndefinedWrapper<
            Category

>>(
      dartName: r'category',
      nullable: false,
      required: false,
      oasName: r'category',
      oasType: r'Category',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      classReflection: CategoryReflection.instance,
      getter: _categoryGetter,
      setter: _categorySetter,
    ),
    namePart: PropertyReflection<PetWithRequiredTags, 
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
    photoUrlsPart: PropertyReflection<PetWithRequiredTags, 
    List<
        
            String

>

>(
      dartName: r'photoUrls',
      nullable: false,
      required: true,
      oasName: r'photoUrls',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<PetWithRequiredTags, 
            String

>(parentReflectionGetter: instanceGetter,),
      getter: _photoUrlsGetter,
      setter: _photoUrlsSetter,
    ),
    tagsPart: PropertyReflection<PetWithRequiredTags, 
    List<
        
            Tag

>

>(
      dartName: r'tags',
      nullable: false,
      required: true,
      oasName: r'tags',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<PetWithRequiredTags, 
            Tag

>(parentReflectionGetter: instanceGetter,classReflection: TagReflection.instance,),
      getter: _tagsGetter,
      setter: _tagsSetter,
    ),
    statusPart: PropertyReflection<PetWithRequiredTags, UndefinedWrapper<
            PetWithRequiredTagsStatusEnum

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
      itemsReflection: ItemsReflection<PetWithRequiredTags, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const PetWithRequiredTagsReflection._({
    required this.modelName,
    required this.className,
    required this.idPart,
    required this.categoryPart,
    required this.namePart,
    required this.photoUrlsPart,
    required this.tagsPart,
    required this.statusPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<PetWithRequiredTags, UndefinedWrapper<
            int

>> idPart;
  static UndefinedWrapper<
            int

> _idGetter(PetWithRequiredTags parent) {
    return parent.id;
  }
  static void _idSetter(PetWithRequiredTags parent, UndefinedWrapper<
            int

> value) {
    parent.id = value;
  }
  final PropertyReflection<PetWithRequiredTags, UndefinedWrapper<
            Category

>> categoryPart;
  static UndefinedWrapper<
            Category

> _categoryGetter(PetWithRequiredTags parent) {
    return parent.category;
  }
  static void _categorySetter(PetWithRequiredTags parent, UndefinedWrapper<
            Category

> value) {
    parent.category = value;
  }
  final PropertyReflection<PetWithRequiredTags, 
            String

> namePart;
  static 
            String

 _nameGetter(PetWithRequiredTags parent) {
    return parent.name;
  }
  static void _nameSetter(PetWithRequiredTags parent, 
            String

 value) {
    parent.name = value;
  }
  final PropertyReflection<PetWithRequiredTags, 
    List<
        
            String

>

> photoUrlsPart;
  static 
    List<
        
            String

>

 _photoUrlsGetter(PetWithRequiredTags parent) {
    return parent.photoUrls;
  }
  static void _photoUrlsSetter(PetWithRequiredTags parent, 
    List<
        
            String

>

 value) {
    parent.photoUrls = value;
  }
  final PropertyReflection<PetWithRequiredTags, 
    List<
        
            Tag

>

> tagsPart;
  static 
    List<
        
            Tag

>

 _tagsGetter(PetWithRequiredTags parent) {
    return parent.tags;
  }
  static void _tagsSetter(PetWithRequiredTags parent, 
    List<
        
            Tag

>

 value) {
    parent.tags = value;
  }
  final PropertyReflection<PetWithRequiredTags, UndefinedWrapper<
            PetWithRequiredTagsStatusEnum

>> statusPart;
  static UndefinedWrapper<
            PetWithRequiredTagsStatusEnum

> _statusGetter(PetWithRequiredTags parent) {
    return parent.status;
  }
  static void _statusSetter(PetWithRequiredTags parent, UndefinedWrapper<
            PetWithRequiredTagsStatusEnum

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
  List<PropertyReflection<PetWithRequiredTags, dynamic>> get properties => [
    idPart,
categoryPart,
namePart,
photoUrlsPart,
tagsPart,
statusPart,
  ];

  final AdditionalPropertiesReflection<PetWithRequiredTags, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<PetWithRequiredTags, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<PetWithRequiredTags, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => PetWithRequiredTags.canDeserialize(src);
  @override
  PetWithRequiredTags Function(Object? src) get deserializeFunction =>
      (src) => PetWithRequiredTags.deserialize(src);

  @override
  Object? Function(PetWithRequiredTags src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of PetWithRequiredTags.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  PetWithRequiredTags example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = PetWithRequiredTags(
      id: () {
        var result = 


            
            


    
    exampleint()


;
        return UndefinedWrapper(result);
      } (),
      category: () {
        var result = 


            
            


    CategoryReflection.instance.example()
    


;
        return UndefinedWrapper(result);
      } (),
      name: () {
        var result = 


            
            


    
    exampleString()


;
        final preSelectedResult = discriminatorExampleResults[namePart.oasName]?.key.key;
        if (preSelectedResult != null) {
          result = preSelectedResult;
        }
        return result;
      } (),
      photoUrls: () {
        var result = 


    exampleList(() { return 


            
            


    
    exampleString()


; })



;
        return result;
      } (),
      tags: () {
        var result = 


    exampleList(() { return 


            
            


    TagReflection.instance.example()
    


; })



;
        return result;
      } (),
      status: () {
        var result = 


            exampleEnum(PetWithRequiredTagsStatusEnum.values)



;
        return UndefinedWrapper(result);
      } (),
      additionalProperties: () { return AdditionalProperties(exampleMap(() => exampleNullable(() =>

exampleObject()



 ) )); }(),
    );
    
    return exampleResult;
  }
}


class PetWithRequiredTagsXmlReflection {
    const PetWithRequiredTagsXmlReflection();
}

