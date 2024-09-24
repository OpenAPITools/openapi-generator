// Model def

import 'package:petstore_api/_internal.dart';


part 'all_of_model_array_any_of.reflection.dart';


/// AllOfModelArrayAnyOfMixin
///
/// Properties:
/// * [linkListColumn1] 
/// * [attributes] 
mixin AllOfModelArrayAnyOfMixin on
  CategoryMixin, $OpenApiObjectMixin {
  UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfLinkListColumn1
> get linkListColumn1;
UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributes
> get attributes;
  
}

/// AllOfModelArrayAnyOf
///
/// Properties:
/// * [name] 
/// * [attributes] 
/// * [id] 
/// * [linkListColumn1] 
class AllOfModelArrayAnyOf with
$OpenApiObjectMixin,
CategoryMixin,
AllOfModelArrayAnyOfMixin {
  @override
  
            String
 name;
  @override
  UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfAttributes
> attributes;
  @override
  UndefinedWrapper<
            int
> id;
  @override
  UndefinedWrapper<
            AllOfModelArrayAnyOfAllOfLinkListColumn1
> linkListColumn1;

  AdditionalProperties<Object
?> additionalProperties;

  

  AllOfModelArrayAnyOf.$all({
        required this.name,
    required this.attributes,
    required this.id,
    required this.linkListColumn1,
    required this.additionalProperties,
    
  });

  AllOfModelArrayAnyOf({
      this.name     =
        
        'default-name'
        
,
  this.attributes = const UndefinedWrapper
        .undefined()
,
  this.id = const UndefinedWrapper
        .undefined()
,
  this.linkListColumn1 = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = AllOfModelArrayAnyOfReflection.instance;
  AllOfModelArrayAnyOfReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
    return super.validate();
  }

  factory AllOfModelArrayAnyOf.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  AllOfModelArrayAnyOf clone() {
    return $reflection.clone(this);
  }
}








