// Model def

import 'package:openapi/_internal.dart';


part 'read_only_first.reflection.dart';
part 'read_only_first.serialization.dart';


/// ReadOnlyFirstMixin
///
/// Properties:
/// * [bar] 
/// * [baz] 
mixin ReadOnlyFirstMixin on 
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get bar;
UndefinedWrapper<
            String
> get baz;
  
}

/// ReadOnlyFirst
///
/// Properties:
/// * [bar] 
/// * [baz] 
class ReadOnlyFirst with
$OpenApiObjectMixin,


ReadOnlyFirstMixin {
  @override
  UndefinedWrapper<
            String
> bar;
  @override
  UndefinedWrapper<
            String
> baz;

  

  

  ReadOnlyFirst.$all({
        required this.bar,
    required this.baz,
    
    
  });

  ReadOnlyFirst({
      this.bar = const UndefinedWrapper
        .undefined()
,
  this.baz = const UndefinedWrapper
        .undefined()
,
    
    
  });

  static const $reflection = ReadOnlyFirstReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$ReadOnlyFirstToMap(this);
  }
  factory ReadOnlyFirst.fromMap(Map<String, dynamic> src) {
    return _$ReadOnlyFirstFromMap(src);
  }
  static ReadOnlyFirst? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return ReadOnlyFirst.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$ReadOnlyFirstCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory ReadOnlyFirst.deserialize(Object? src) {
    return _$ReadOnlyFirstDeserialize(src);
  }
  static ReadOnlyFirst? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return ReadOnlyFirst.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$ReadOnlyFirstCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$ReadOnlyFirstSerialize(this);
  }
}




