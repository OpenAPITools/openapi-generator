// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'pet.dart';

// **************************************************************************
// JaguarSerializerGenerator
// **************************************************************************

abstract class _$PetSerializer implements Serializer<Pet> {
  Serializer<Category> __categorySerializer;
  Serializer<Category> get _categorySerializer =>
      __categorySerializer ??= new CategorySerializer();
  Serializer<Tag> __tagSerializer;
  Serializer<Tag> get _tagSerializer => __tagSerializer ??= new TagSerializer();
  @override
  Map<String, dynamic> toMap(Pet model) {
    if (model == null) return null;
    Map<String, dynamic> ret = <String, dynamic>{};
    setMapValue(ret, 'id', model.id);
    setMapValue(ret, 'category', _categorySerializer.toMap(model.category));
    setMapValue(ret, 'name', model.name);
    setMapValue(ret, 'photoUrls',
        codeIterable(model.photoUrls, (val) => val as String));
    setMapValue(ret, 'tags',
        codeIterable(model.tags, (val) => _tagSerializer.toMap(val as Tag)));
    setMapValue(ret, 'status', model.status);
    return ret;
  }

  @override
  Pet fromMap(Map map) {
    if (map == null) return null;
    final obj = new Pet(
        id: map['id'] as int ?? getJserDefault('id'),
        category: _categorySerializer.fromMap(map['category'] as Map) ??
            getJserDefault('category'),
        name: map['name'] as String ?? getJserDefault('name'),
        photoUrls: codeIterable<String>(
                map['photoUrls'] as Iterable, (val) => val as String) ??
            getJserDefault('photoUrls'),
        tags: codeIterable<Tag>(map['tags'] as Iterable,
                (val) => _tagSerializer.fromMap(val as Map)) ??
            getJserDefault('tags'),
        status: map['status'] as String ?? getJserDefault('status'));
    return obj;
  }
}
