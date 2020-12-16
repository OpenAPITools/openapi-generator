import 'package:openapi/model/animal.dart';
import 'package:openapi/model/cat_all_of.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'cat.g.dart';

abstract class Cat implements Built<Cat, CatBuilder> {

    @nullable
    @BuiltValueField(wireName: r'className')
    String get className;

    @nullable
    @BuiltValueField(wireName: r'color')
    String get color;

    @nullable
    @BuiltValueField(wireName: r'declawed')
    bool get declawed;

    // Boilerplate code needed to wire-up generated code
    Cat._();

    factory Cat([updates(CatBuilder b)]) = _$Cat;
    static Serializer<Cat> get serializer => _$catSerializer;
}

