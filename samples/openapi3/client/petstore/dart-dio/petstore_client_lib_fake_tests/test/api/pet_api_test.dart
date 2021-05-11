import 'package:built_collection/built_collection.dart';
import 'package:dio/dio.dart';
import 'package:http_mock_adapter/http_mock_adapter.dart';
import 'package:openapi/api.dart';
import 'package:openapi/api/pet_api.dart';
import 'package:openapi/model/category.dart';
import 'package:openapi/model/pet.dart';
import 'package:openapi/model/tag.dart';
import 'package:test/test.dart';

void main() {
  const photo1 = 'https://localhost/photo1.jpg';
  const photo2 = 'https://localhost/photo2.jpg';

  Openapi client;
  DioAdapter server;

  setUp(() {
    server = DioAdapter();
    client = Openapi(dio: Dio()..httpClientAdapter = server);
  });

  tearDown(() {
    server.close();
  });

  group(PetApi, () {
    group('getPetById', () {
      test('complete', () async {
        server.onGet(
          '/pet/5',
          (request) => request.reply(200, {
            'id': 5,
            'name': 'Paula',
            'status': 'sold',
            'category': {
              'id': 1,
              'name': 'dog',
            },
            'photoUrls': [
              '$photo1',
              '$photo2',
            ],
            'tags': [
              {
                'id': 3,
                'name': 'smart',
              },
              {
                'id': 4,
                'name': 'cute',
              },
            ]
          }),
        );

        final response = await client.getPetApi().getPetById(5);

        expect(response.statusCode, 200);
        expect(response.data, isNotNull);
        expect(response.data.id, 5);
        expect(response.data.name, 'Paula');
        expect(response.data.status, PetStatusEnum.sold);
        expect(response.data.category.id, 1);
        expect(response.data.category.name, 'dog');
        expect(response.data.photoUrls.length, 2);
        expect(response.data.tags.length, 2);
      });

      test('minimal', () async {
        server.onGet(
          '/pet/5',
          (request) => request.reply(200, {
            'id': 5,
            'name': 'Paula',
            'photoUrls': <String>[],
          }),
        );

        final response = await client.getPetApi().getPetById(5);

        expect(response.statusCode, 200);
        expect(response.data, isNotNull);
        expect(response.data.id, 5);
        expect(response.data.name, 'Paula');
        expect(response.data.status, isNull);
        expect(response.data.category, isNull);
        expect(response.data.photoUrls, isNotNull);
        expect(response.data.photoUrls, isEmpty);
      });
    });

    group('addPet', () {
      test('complete', () async {
        server.onPost(
          '/pet',
          (request) => request.reply(200, ''),
          data: {
            'id': 5,
            'name': 'Paula',
            'status': 'sold',
            'category': {
              'id': 1,
              'name': 'dog',
            },
            'photoUrls': [
              '$photo1',
              '$photo2',
            ],
            'tags': [
              {
                'id': 3,
                'name': 'smart',
              },
              {
                'id': 4,
                'name': 'cute',
              },
            ]
          },
          headers: {
            'content-type': 'application/json',
            'content-length': 204,
          },
        );

        final response = await client.getPetApi().addPet(Pet((p) => p
          ..id = 5
          ..name = 'Paula'
          ..status = PetStatusEnum.sold
          ..category = (CategoryBuilder()
            ..id = 1
            ..name = 'dog')
          ..photoUrls = SetBuilder<String>(<String>[photo1, photo2])
          ..tags = ListBuilder<Tag>(<Tag>[
            Tag((t) => t
              ..id = 3
              ..name = 'smart'),
            Tag((t) => t
              ..id = 4
              ..name = 'cute'),
          ])));

        expect(response.statusCode, 200);
      });

      test('minimal', () async {
        server.onPost(
          '/pet',
          (request) => request.reply(200, ''),
          data: {
            'id': 5,
            'name': 'Paula',
            'photoUrls': <String>[],
          },
          headers: {
            'content-type': 'application/json',
            'content-length': 38,
          },
        );

        final response = await client.getPetApi().addPet(Pet((p) => p
          ..id = 5
          ..name = 'Paula'));

        expect(response.statusCode, 200);
      });
    });

    group('getMultiplePets', () {
      test('findByStatus', () async {
        server.onRoute(
          '/pet/findByStatus',
          (request) => request.reply(200, [
            {
              'id': 5,
              'name': 'Paula',
              'status': 'sold',
              'photoUrls': <String>[],
            },
            {
              'id': 1,
              'name': 'Mickey',
              'status': 'available',
              'photoUrls': <String>[],
            },
          ]),
          request: Request(
            method: RequestMethods.get,
            queryParameters: <String, dynamic>{
              'status': <String>[
                'available',
                'sold',
              ],
            },
          ),
        );

        final response = await client.getPetApi().findPetsByStatus(
              ListBuilder<String>(<String>[
                PetStatusEnum.available.name,
                PetStatusEnum.sold.name,
              ]).build(),
            );

        expect(response.statusCode, 200);
        expect(response.data, isNotNull);
        expect(response.data.length, 2);
        expect(response.data[0].id, 5);
        expect(response.data[0].name, 'Paula');
        expect(response.data[0].status, PetStatusEnum.sold);
        expect(response.data[1].id, 1);
        expect(response.data[1].name, 'Mickey');
        expect(response.data[1].status, PetStatusEnum.available);
      });
    });
  });
}
