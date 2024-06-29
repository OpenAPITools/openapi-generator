// Model def

import 'package:openapi/_internal.dart';


part 'drawing.reflection.dart';
part 'drawing.serialization.dart';


/// DrawingMixin
///
/// Properties:
/// * [mainShape] 
/// * [shapeOrNull] 
/// * [nullableShape] 
/// * [shapes] 
mixin DrawingMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            Shape
> get mainShape;
UndefinedWrapper<
            ShapeOrNull
?> get shapeOrNull;
UndefinedWrapper<
            NullableShape
?> get nullableShape;
UndefinedWrapper<
    List<
        
            Shape
>
> get shapes;
  
}

/// Drawing
///
/// Properties:
/// * [mainShape] 
/// * [shapeOrNull] 
/// * [nullableShape] 
/// * [shapes] 
class Drawing with
$OpenApiObjectMixin,


DrawingMixin {
  @override
  UndefinedWrapper<
            Shape
> mainShape;
  @override
  UndefinedWrapper<
            ShapeOrNull
?> shapeOrNull;
  @override
  UndefinedWrapper<
            NullableShape
?> nullableShape;
  @override
  UndefinedWrapper<
    List<
        
            Shape
>
> shapes;

  AdditionalProperties<
            Fruit
> additionalProperties;

  

  Drawing.$all({
        required this.mainShape,
    required this.shapeOrNull,
    required this.nullableShape,
    required this.shapes,
    required this.additionalProperties,
    
  });

  Drawing({
      this.mainShape = const UndefinedWrapper
        .undefined()
,
  this.shapeOrNull = const UndefinedWrapper
        .undefined()
,
  this.nullableShape = const UndefinedWrapper
        .undefined()
,
  this.shapes = const UndefinedWrapper
        .undefined()
,
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = DrawingReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$DrawingToMap(this);
  }
  factory Drawing.fromMap(Map<String, dynamic> src) {
    return _$DrawingFromMap(src);
  }
  static Drawing? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return Drawing.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$DrawingCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory Drawing.deserialize(Object? src) {
    return _$DrawingDeserialize(src);
  }
  static Drawing? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return Drawing.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$DrawingCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$DrawingSerialize(this);
  }
}




