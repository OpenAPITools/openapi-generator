export '_exports.dart';

export 'reflection.dart';
export 'xml_reflection.dart';
export 'context.dart';
export 'json_extensions.dart';
export 'xml_extensions.dart';
export 'examples.dart';
export 'primitive_reflection.dart';
export 'model_reflection.dart';
export 'container_reflection.dart';
export 'generated_reflections.dart';

Never throwArgumentMismatch(Type type, Object? value) {
  throw ArgumentError.value(value, 'value',
      'Value was expected to be of type ($type), but instead was of type (${value.runtimeType})');
}

Never throwArgumentRequired(String name) {
  throw ArgumentError.value(name, 'name', 'Value was required, but its value was undefined');
}

TOther? ifNotNull<T, TOther>(T? src, TOther? Function(T) fn) {
  if (src == null) {
    return null;
  }
  return fn(src);
}
