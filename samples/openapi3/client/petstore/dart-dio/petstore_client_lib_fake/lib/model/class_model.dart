import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'class_model.g.dart';

abstract class ClassModel implements Built<ClassModel, ClassModelBuilder> {

    @nullable
    @BuiltValueField(wireName: r'_class')
    String get class_;

    // Boilerplate code needed to wire-up generated code
    ClassModel._();

    factory ClassModel([updates(ClassModelBuilder b)]) = _$ClassModel;
    static Serializer<ClassModel> get serializer => _$classModelSerializer;
}

