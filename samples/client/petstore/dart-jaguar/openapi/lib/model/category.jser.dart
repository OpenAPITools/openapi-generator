// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'category.dart';

// **************************************************************************
// JaguarSerializerGenerator
// **************************************************************************

abstract class _$CategorySerializer implements Serializer<Category> {
  @override
  Map<String, dynamic> toMap(Category model) {
    if (model == null) return null;
    Map<String, dynamic> ret = <String, dynamic>{};
    setMapValue(ret, 'id', model.id);
    setMapValue(ret, 'name', model.name);
    return ret;
  }

  @override
  Category fromMap(Map map) {
    if (map == null) return null;
    final obj = new Category(
        id: map['id'] as int ?? getJserDefault('id'),
        name: map['name'] as String ?? getJserDefault('name'));
    return obj;
  }
}
