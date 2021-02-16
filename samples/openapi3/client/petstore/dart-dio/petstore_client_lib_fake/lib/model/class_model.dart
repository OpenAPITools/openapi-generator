//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'class_model.g.dart';

abstract class ClassModel implements Built<ClassModel, ClassModelBuilder> {

    @nullable
    @BuiltValueField(wireName: r'_class')
    String get class_;

    // Boilerplate code needed to wire-up generated code
    ClassModel._();

    static void _initializeBuilder(ClassModelBuilder b) => b;

    factory ClassModel([void updates(ClassModelBuilder b)]) = _$ClassModel;
    static Serializer<ClassModel> get serializer => _$classModelSerializer;
}

