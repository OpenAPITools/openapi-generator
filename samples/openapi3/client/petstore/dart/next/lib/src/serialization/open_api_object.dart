import 'package:meta/meta.dart';

/// A marker mixin to be the base class for all openapi schemas of type `object`
mixin $OpenApiObjectMixin {
  @mustCallSuper
  bool validate() => true;
}

/// An openapi object that has no properties.
class EmptyOpenApiObject with $OpenApiObjectMixin {
  const EmptyOpenApiObject();
}
