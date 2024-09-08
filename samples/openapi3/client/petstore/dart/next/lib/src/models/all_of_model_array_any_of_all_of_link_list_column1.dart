// Model def

import 'package:petstore_api/_internal.dart';


part 'all_of_model_array_any_of_all_of_link_list_column1.reflection.dart';


/// AllOfModelArrayAnyOfAllOfLinkListColumn1Mixin
///
/// Properties:
/// * [value] 
mixin AllOfModelArrayAnyOfAllOfLinkListColumn1Mixin on
  $OpenApiObjectMixin {
  
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value
>
 get value;
  
}

/// AllOfModelArrayAnyOfAllOfLinkListColumn1
///
/// Properties:
/// * [value] 
class AllOfModelArrayAnyOfAllOfLinkListColumn1 with
$OpenApiObjectMixin,

AllOfModelArrayAnyOfAllOfLinkListColumn1Mixin {
  @override
  
    List<
        
            AllOfModelArrayAnyOfAllOfLinkListColumn1Value
>
 value;

  AdditionalProperties<Object
?> additionalProperties;

  

  AllOfModelArrayAnyOfAllOfLinkListColumn1.$all({
        required this.value,
    required this.additionalProperties,
    
  });

  AllOfModelArrayAnyOfAllOfLinkListColumn1({
    required  this.value     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection.instance;
  AllOfModelArrayAnyOfAllOfLinkListColumn1Reflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory AllOfModelArrayAnyOfAllOfLinkListColumn1.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  AllOfModelArrayAnyOfAllOfLinkListColumn1 clone() {
    return $reflection.clone(this);
  }
}







