export '_exports.dart';

export 'json_reflection.dart';
export 'xml_reflection.dart';

export 'json_extensions.dart';
export 'xml_extensions.dart';
export 'examples.dart';

Never throwArgumentMismatch(Type type, Object? value) {
    throw ArgumentError.value(value, 'value', 'Value was expected to be of type ($type), but instead was of type (${value.runtimeType})');
}