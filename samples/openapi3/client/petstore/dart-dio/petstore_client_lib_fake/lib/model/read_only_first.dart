import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'read_only_first.g.dart';

abstract class ReadOnlyFirst implements Built<ReadOnlyFirst, ReadOnlyFirstBuilder> {

    @nullable
    @BuiltValueField(wireName: r'bar')
    String get bar;

    @nullable
    @BuiltValueField(wireName: r'baz')
    String get baz;

    // Boilerplate code needed to wire-up generated code
    ReadOnlyFirst._();

    static void _initializeBuilder(ReadOnlyFirstBuilder b) => b;

    factory ReadOnlyFirst([updates(ReadOnlyFirstBuilder b)]) = _$ReadOnlyFirst;
    static Serializer<ReadOnlyFirst> get serializer => _$readOnlyFirstSerializer;
}

