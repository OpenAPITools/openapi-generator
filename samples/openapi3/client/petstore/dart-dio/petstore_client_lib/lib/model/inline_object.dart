import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'inline_object.g.dart';

abstract class InlineObject implements Built<InlineObject, InlineObjectBuilder> {

    /// Updated name of the pet
    @nullable
    @BuiltValueField(wireName: r'name')
    String get name;

    /// Updated status of the pet
    @nullable
    @BuiltValueField(wireName: r'status')
    String get status;

    // Boilerplate code needed to wire-up generated code
    InlineObject._();

    static void _initializeBuilder(InlineObjectBuilder b) => b;

    factory InlineObject([updates(InlineObjectBuilder b)]) = _$InlineObject;
    static Serializer<InlineObject> get serializer => _$inlineObjectSerializer;
}

