// Model def

import 'package:petstore_api/_internal.dart';


part 'outer_composite.reflection.dart';
part 'outer_composite.serialization.dart';


/// OuterCompositeMixin
///
/// Properties:
/// * [myNumber] 
/// * [myString] 
/// * [myBoolean] 
mixin OuterCompositeMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            num

> get myNumber;
UndefinedWrapper<
            String

> get myString;
UndefinedWrapper<
            bool

> get myBoolean;
  
}

/// OuterComposite
///
/// Properties:
/// * [myNumber] 
/// * [myString] 
/// * [myBoolean] 
class OuterComposite with
$OpenApiObjectMixin,

OuterCompositeMixin {
  @override
  UndefinedWrapper<
            num

> myNumber;
  @override
  UndefinedWrapper<
            String

> myString;
  @override
  UndefinedWrapper<
            bool

> myBoolean;

  AdditionalProperties<Object

?> additionalProperties;

  

  OuterComposite.$all({
        required this.myNumber,
    required this.myString,
    required this.myBoolean,
    required this.additionalProperties,
    
  });

  OuterComposite({
      this.myNumber = const UndefinedWrapper
        .undefined()
,
  this.myString = const UndefinedWrapper
        .undefined()
,
  this.myBoolean = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = OuterCompositeReflection.instance;
  OuterCompositeReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$OuterCompositeToMap(this);
  }
  factory OuterComposite.fromMap(Map<String, dynamic> src) {
    return _$OuterCompositeFromMap(src);
  }
  static OuterComposite? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return OuterComposite.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$OuterCompositeCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory OuterComposite.deserialize(Object? src) {
    return _$OuterCompositeDeserialize(src);
  }
  static OuterComposite? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return OuterComposite.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$OuterCompositeCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$OuterCompositeSerialize(this);
  }
}




