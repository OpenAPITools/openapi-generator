import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'inline_object4.g.dart';

abstract class InlineObject4 implements Built<InlineObject4, InlineObject4Builder> {

    /// field1
    @nullable
    @BuiltValueField(wireName: r'param')
    String get param;

    /// field2
    @nullable
    @BuiltValueField(wireName: r'param2')
    String get param2;

    // Boilerplate code needed to wire-up generated code
    InlineObject4._();

    static void _initializeBuilder(InlineObject4Builder b) => b;

    factory InlineObject4([updates(InlineObject4Builder b)]) = _$InlineObject4;
    static Serializer<InlineObject4> get serializer => _$inlineObject4Serializer;
}

