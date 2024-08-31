import 'package:petstore_api/_internal.dart';

/// A marker mixin to be the base class for all openapi schemas of type `object` or schemas containing `oneof`, `anyof`.
///
/// Where an `object` is a value that can be serialized/deserialized from a json map.
///
/// Every openapi object MUST implement `$classReflection`, and optionally implement validate.
mixin $OpenApiObjectMixin {
  @mustCallSuper
  bool validate() => true;

  ModelReflection get $classReflection;
}