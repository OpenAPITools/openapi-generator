import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model_client.g.dart';

abstract class ModelClient implements Built<ModelClient, ModelClientBuilder> {

    @nullable
    @BuiltValueField(wireName: r'client')
    String get client;

    // Boilerplate code needed to wire-up generated code
    ModelClient._();

    static void _initializeBuilder(ModelClientBuilder b) => b;

    factory ModelClient([updates(ModelClientBuilder b)]) = _$ModelClient;
    static Serializer<ModelClient> get serializer => _$modelClientSerializer;
}

