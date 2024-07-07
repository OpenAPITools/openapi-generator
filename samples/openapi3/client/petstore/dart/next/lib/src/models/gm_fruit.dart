// Model def

import 'package:petstore_api/_internal.dart';


part 'gm_fruit.reflection.dart';
part 'gm_fruit.serialization.dart';


/// GmFruitMixin
///
/// Properties:
/// * [color] 
mixin GmFruitMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get color;
  
  UndefinedWrapper<
            Apple
?> get anyOf0;
  UndefinedWrapper<
            Banana
> get anyOf1;
}

/// GmFruit
///
/// Properties:
/// * [color] 
class GmFruit with
$OpenApiObjectMixin,

GmFruitMixin {
  @override
  UndefinedWrapper<
            String
> color;



  
  @override
  UndefinedWrapper<
            Apple
?> anyOf0;
  
  @override
  UndefinedWrapper<
            Banana
> anyOf1;
  

  GmFruit.$all({
        required this.color,
    
    
    required this.anyOf0,
    required this.anyOf1,
  });

  GmFruit({
      this.color = const UndefinedWrapper
        .undefined()
,
    
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
  });

  static const $reflection = GmFruitReflection.instance;
  GmFruitReflection get $classReflection => $reflection;

  @override
  bool validate() {
      
      final anyOfs = [anyOf0,anyOf1,].where((e) => e.isDefined).take(1);
      if (anyOfs.length == 0) {
        // there must be AT LEAST one "anyOf" schema.
        return false;
      }
      
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$GmFruitToMap(this);
  }
  factory GmFruit.fromMap(Map<String, dynamic> src) {
    return _$GmFruitFromMap(src);
  }
  static GmFruit? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return GmFruit.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$GmFruitCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory GmFruit.deserialize(Object? src) {
    return _$GmFruitDeserialize(src);
  }
  static GmFruit? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return GmFruit.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$GmFruitCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$GmFruitSerialize(this);
  }
}




