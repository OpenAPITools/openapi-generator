// Model def

import 'package:openapi/_internal.dart';


part 'fruit.reflection.dart';
part 'fruit.serialization.dart';


/// FruitMixin
///
/// Properties:
/// * [color] 
mixin FruitMixin on 
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get color;
  
  UndefinedWrapper<
            Apple
?> get oneOf0;
  UndefinedWrapper<
            Banana
> get oneOf1;
}

/// Fruit
///
/// Properties:
/// * [color] 
class Fruit with
$OpenApiObjectMixin,


FruitMixin {
  @override
  UndefinedWrapper<
            String
> color;

  

  
  @override
  UndefinedWrapper<
            Apple
?> oneOf0;
  
  @override
  UndefinedWrapper<
            Banana
> oneOf1;
  

  Fruit.$all({
        required this.color,
    
    
    required this.oneOf0,
    required this.oneOf1,
  });

  Fruit({
      this.color = const UndefinedWrapper
        .undefined()
,
    
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
  });

  static const $reflection = FruitReflection.instance;

  @override
  bool validate() {
      final oneOfs = [oneOf0,oneOf1,].where((e) => e.isDefined).take(2);
      if (oneOfs.length != 1) {
        // there must be EXACTLY one "oneOf" schema.
        return false;
      }
      
      
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$FruitToMap(this);
  }
  factory Fruit.fromMap(Map<String, dynamic> src) {
    return _$FruitFromMap(src);
  }
  static Fruit? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Fruit.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$FruitCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Fruit.deserialize(Object? src) {
    return _$FruitDeserialize(src);
  }
  static Fruit? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Fruit.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$FruitCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$FruitSerialize(this);
  }
}




