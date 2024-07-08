// Model def

import 'package:petstore_api/_internal.dart';


part 'array_test.reflection.dart';
part 'array_test.serialization.dart';


/// ArrayTestMixin
///
/// Properties:
/// * [arrayOfString] 
/// * [arrayArrayOfInteger] 
/// * [arrayArrayOfModel] 
mixin ArrayTestMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
    List<
        
            String

>

> get arrayOfString;
UndefinedWrapper<
    List<
        
    List<
        
            int

>

>

> get arrayArrayOfInteger;
UndefinedWrapper<
    List<
        
    List<
        
            ReadOnlyFirst

>

>

> get arrayArrayOfModel;
  
}

/// ArrayTest
///
/// Properties:
/// * [arrayOfString] 
/// * [arrayArrayOfInteger] 
/// * [arrayArrayOfModel] 
class ArrayTest with
$OpenApiObjectMixin,

ArrayTestMixin {
  @override
  UndefinedWrapper<
    List<
        
            String

>

> arrayOfString;
  @override
  UndefinedWrapper<
    List<
        
    List<
        
            int

>

>

> arrayArrayOfInteger;
  @override
  UndefinedWrapper<
    List<
        
    List<
        
            ReadOnlyFirst

>

>

> arrayArrayOfModel;

  AdditionalProperties<Object

?> additionalProperties;

  

  ArrayTest.$all({
        required this.arrayOfString,
    required this.arrayArrayOfInteger,
    required this.arrayArrayOfModel,
    required this.additionalProperties,
    
  });

  ArrayTest({
      this.arrayOfString = const UndefinedWrapper
        .undefined()
,
  this.arrayArrayOfInteger = const UndefinedWrapper
        .undefined()
,
  this.arrayArrayOfModel = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = ArrayTestReflection.instance;
  ArrayTestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$ArrayTestToMap(this);
  }
  factory ArrayTest.fromMap(Map<String, dynamic> src) {
    return _$ArrayTestFromMap(src);
  }
  static ArrayTest? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ArrayTest.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ArrayTestCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ArrayTest.deserialize(Object? src) {
    return _$ArrayTestDeserialize(src);
  }
  static ArrayTest? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ArrayTest.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ArrayTestCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$ArrayTestSerialize(this);
  }
}




