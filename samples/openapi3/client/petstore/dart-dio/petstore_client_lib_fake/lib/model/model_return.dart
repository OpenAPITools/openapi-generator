import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model_return.g.dart';

abstract class ModelReturn implements Built<ModelReturn, ModelReturnBuilder> {

    @nullable
    @BuiltValueField(wireName: r'return')
    int get return_;

    // Boilerplate code needed to wire-up generated code
    ModelReturn._();

    factory ModelReturn([updates(ModelReturnBuilder b)]) = _$ModelReturn;
    static Serializer<ModelReturn> get serializer => _$modelReturnSerializer;
}

