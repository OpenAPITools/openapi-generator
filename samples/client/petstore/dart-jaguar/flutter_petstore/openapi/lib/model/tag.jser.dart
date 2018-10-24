// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'tag.dart';

// **************************************************************************
// JaguarSerializerGenerator
// **************************************************************************

abstract class _$TagSerializer implements Serializer<Tag> {
  @override
  Map<String, dynamic> toMap(Tag model) {
    if (model == null) return null;
    Map<String, dynamic> ret = <String, dynamic>{};
    setMapValue(ret, 'id', model.id);
    setMapValue(ret, 'name', model.name);
    return ret;
  }

  @override
  Tag fromMap(Map map) {
    if (map == null) return null;
    final obj = new Tag(
        id: map['id'] as int ?? getJserDefault('id'),
        name: map['name'] as String ?? getJserDefault('name'));
    return obj;
  }
}
