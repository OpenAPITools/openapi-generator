// Model def

import 'package:openapi/_internal.dart';


part 'foo.reflection.dart';
part 'foo.serialization.dart';


/// FooMixin
///
/// Properties:
/// * [bar] 
mixin FooMixin on 
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get bar;
  
}

/// Foo
///
/// Properties:
/// * [bar] 
class Foo with
$OpenApiObjectMixin,


FooMixin {
  @override
  UndefinedWrapper<
            String
> bar;

  

  

  Foo.$all({
        required this.bar,
    
    
  });

  Foo({
      this.bar = const UndefinedWrapper
    (
        
        'bar'
    )
    
,
    
    
  });

  static const $reflection = FooReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$FooToMap(this);
  }
  factory Foo.fromMap(Map<String, dynamic> src) {
    return _$FooFromMap(src);
  }
  static Foo? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Foo.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$FooCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Foo.deserialize(Object? src) {
    return _$FooDeserialize(src);
  }
  static Foo? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Foo.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$FooCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$FooSerialize(this);
  }
}




