// Model reflection

part of 'new_pet.dart';


//class reflection

class NewPetReflection extends ClassReflection<NewPet> {
  static NewPetReflection instanceGetter() => instance;
  static const instance = NewPetReflection._(
    modelName: r'NewPet',
    className: r'NewPet',
    idPart: PropertyReflection<NewPet, UndefinedWrapper<
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
    categoryInlineAllofPart: PropertyReflection<NewPet, UndefinedWrapper<
            NewPetCategoryInlineAllof

>>(
      dartName: r'categoryInlineAllof',
      nullable: false,
      required: false,
      oasName: r'category_inline_allof',
      oasType: r'NewPetCategoryInlineAllof',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      classReflection: NewPetCategoryInlineAllofReflection.instance,
      getter: _categoryInlineAllofGetter,
      setter: _categoryInlineAllofSetter,
    ),
    categoryAllOfRefPart: PropertyReflection<NewPet, UndefinedWrapper<
            Category

>>(
      dartName: r'categoryAllOfRef',
      nullable: false,
      required: false,
      oasName: r'category_allOf_ref',
      oasType: r'Category',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      classReflection: CategoryReflection.instance,
      getter: _categoryAllOfRefGetter,
      setter: _categoryAllOfRefSetter,
    ),
    namePart: PropertyReflection<NewPet, 
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
    photoUrlsPart: PropertyReflection<NewPet, 
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
      itemsReflection: ItemsReflection<NewPet, 
            String

>(parentReflectionGetter: instanceGetter,),
      getter: _photoUrlsGetter,
      setter: _photoUrlsSetter,
    ),
    tagsPart: PropertyReflection<NewPet, UndefinedWrapper<
    List<
        
            Tag

>

>>(
      dartName: r'tags',
      nullable: false,
      required: false,
      oasName: r'tags',
      oasType: r'array',
      pattern: null,
      parentReflectionGetter:  instanceGetter,
      isDiscriminator: false,
      itemsReflection: ItemsReflection<NewPet, 
            Tag

>(parentReflectionGetter: instanceGetter,classReflection: TagReflection.instance,),
      getter: _tagsGetter,
      setter: _tagsSetter,
    ),
    statusPart: PropertyReflection<NewPet, UndefinedWrapper<
            NewPetStatusEnum

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
      itemsReflection: ItemsReflection<NewPet, Object

?>(parentReflectionGetter: instanceGetter,),
          ),
  );
  const NewPetReflection._({
    required this.modelName,
    required this.className,
    required this.idPart,
    required this.categoryInlineAllofPart,
    required this.categoryAllOfRefPart,
    required this.namePart,
    required this.photoUrlsPart,
    required this.tagsPart,
    required this.statusPart,
    this.discriminatorKey,
    this.discriminatorMappings = const {},
    this.discriminatorImplicitMappings = const {},
        
    required this.additionalPropertiesPart,
  });

  final PropertyReflection<NewPet, UndefinedWrapper<
            int

>> idPart;
  static UndefinedWrapper<
            int

> _idGetter(NewPet parent) {
    return parent.id;
  }
  static void _idSetter(NewPet parent, UndefinedWrapper<
            int

> value) {
    parent.id = value;
  }
  final PropertyReflection<NewPet, UndefinedWrapper<
            NewPetCategoryInlineAllof

>> categoryInlineAllofPart;
  static UndefinedWrapper<
            NewPetCategoryInlineAllof

> _categoryInlineAllofGetter(NewPet parent) {
    return parent.categoryInlineAllof;
  }
  static void _categoryInlineAllofSetter(NewPet parent, UndefinedWrapper<
            NewPetCategoryInlineAllof

> value) {
    parent.categoryInlineAllof = value;
  }
  final PropertyReflection<NewPet, UndefinedWrapper<
            Category

>> categoryAllOfRefPart;
  static UndefinedWrapper<
            Category

> _categoryAllOfRefGetter(NewPet parent) {
    return parent.categoryAllOfRef;
  }
  static void _categoryAllOfRefSetter(NewPet parent, UndefinedWrapper<
            Category

> value) {
    parent.categoryAllOfRef = value;
  }
  final PropertyReflection<NewPet, 
            String

> namePart;
  static 
            String

 _nameGetter(NewPet parent) {
    return parent.name;
  }
  static void _nameSetter(NewPet parent, 
            String

 value) {
    parent.name = value;
  }
  final PropertyReflection<NewPet, 
    List<
        
            String

>

> photoUrlsPart;
  static 
    List<
        
            String

>

 _photoUrlsGetter(NewPet parent) {
    return parent.photoUrls;
  }
  static void _photoUrlsSetter(NewPet parent, 
    List<
        
            String

>

 value) {
    parent.photoUrls = value;
  }
  final PropertyReflection<NewPet, UndefinedWrapper<
    List<
        
            Tag

>

>> tagsPart;
  static UndefinedWrapper<
    List<
        
            Tag

>

> _tagsGetter(NewPet parent) {
    return parent.tags;
  }
  static void _tagsSetter(NewPet parent, UndefinedWrapper<
    List<
        
            Tag

>

> value) {
    parent.tags = value;
  }
  final PropertyReflection<NewPet, UndefinedWrapper<
            NewPetStatusEnum

>> statusPart;
  static UndefinedWrapper<
            NewPetStatusEnum

> _statusGetter(NewPet parent) {
    return parent.status;
  }
  static void _statusSetter(NewPet parent, UndefinedWrapper<
            NewPetStatusEnum

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
  List<PropertyReflection<NewPet, dynamic>> get properties => [
    idPart,
categoryInlineAllofPart,
categoryAllOfRefPart,
namePart,
photoUrlsPart,
tagsPart,
statusPart,
  ];

  final AdditionalPropertiesReflection<NewPet, Object

?> additionalPropertiesPart;

  
  
  @override
  List<PartReflection<NewPet, dynamic>> get parts => [
    ...super.parts,
    additionalPropertiesPart,
  ];
  @override
  List<AllOfReflection<NewPet, dynamic>> get allOfs => [
    
  ];


  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => NewPet.canDeserialize(src);
  @override
  NewPet Function(Object? src) get deserializeFunction =>
      (src) => NewPet.deserialize(src);

  @override
  Object? Function(NewPet src) get serializeFunction =>
      (src) => src.serialize();

  /// Gets an example of NewPet.
  /// - [discriminators]: The set of aggregated discriminator properties in the target type, accessed by
  ///  calling [aggregatedDiscriminators].
  NewPet example({AggregatedDiscriminatorsResult? discriminators, Map<DiscriminatorKey, MapEntry<DiscriminatorValue, ClassReflection>>
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

    final exampleResult = NewPet(
      id: () {
        var result = 


            
            


    
    exampleint()


;
        return UndefinedWrapper(result);
      } (),
      categoryInlineAllof: () {
        var result = 


            
            


    NewPetCategoryInlineAllofReflection.instance.example()
    


;
        return UndefinedWrapper(result);
      } (),
      categoryAllOfRef: () {
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
        return UndefinedWrapper(result);
      } (),
      status: () {
        var result = 


            exampleEnum(NewPetStatusEnum.values)



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


class NewPetXmlReflection {
    const NewPetXmlReflection();
}

