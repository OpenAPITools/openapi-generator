import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'cat_all_of.g.dart';

abstract class CatAllOf implements Built<CatAllOf, CatAllOfBuilder> {

    @nullable
    @BuiltValueField(wireName: r'declawed')
    bool get declawed;

    // Boilerplate code needed to wire-up generated code
    CatAllOf._();

    factory CatAllOf([updates(CatAllOfBuilder b)]) = _$CatAllOf;
    static Serializer<CatAllOf> get serializer => _$catAllOfSerializer;
}

