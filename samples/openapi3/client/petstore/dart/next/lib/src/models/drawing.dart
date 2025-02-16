// Model def

import 'package:petstore_api/_internal.dart';


part 'drawing.reflection.dart';


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
> get shapeOrNull;
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
> shapeOrNull;
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
    AdditionalProperties<
            Fruit
>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = DrawingReflection.instance;
  DrawingReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory Drawing.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  Drawing clone() {
    return $reflection.clone(this);
  }
}
















