import 'package:built_collection/built_collection.dart';
import 'package:dio/dio.dart';
import 'package:dio/src/parameter.dart';
import 'package:http_mock_adapter/http_mock_adapter.dart';
import 'package:http_parser/http_parser.dart';
import 'package:openapi/openapi.dart';
import 'package:test/test.dart';

void main() {
  const photo1 = 'https://localhost/photo1.jpg';
  const photo2 = 'https://localhost/photo2.jpg';

  Openapi client;
  DioAdapter tester;

  setUp(() {
    client = Openapi(dio: Dio());
    tester = DioAdapter(dio: client.dio);
  });

  tearDown(() {
    tester.close();
  });

  group(PetApi, () {
    group('getPetById', () {
      test('complete', () async {
        tester.onGet(
          '/pet/5',
          (server) => server.reply(200, {
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

        final response = await client.getPetApi().getPetById(petId: 5);

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
        tester.onGet(
          '/pet/5',
          (server) => server.reply(200, {
            'id': 5,
            'name': 'Paula',
            'photoUrls': <String>[],
          }),
        );

        final response = await client.getPetApi().getPetById(petId: 5);

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
        tester.onPost(
          '/pet',
          (server) => server.reply(200, ''),
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
          headers: <String, dynamic>{
            Headers.contentTypeHeader: Matchers.pattern('application/json'),
            Headers.contentLengthHeader: Matchers.integer,
          },
        );

        final response = await client.getPetApi().addPet(
            pet: Pet((p) => p
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
        tester.onPost(
          '/pet',
          (server) => server.reply(200, ''),
          data: {
            'id': 5,
            'name': 'Paula',
            'photoUrls': <String>[],
          },
          headers: <String, dynamic>{
            Headers.contentTypeHeader: Matchers.pattern('application/json'),
            Headers.contentLengthHeader: Matchers.integer,
          },
        );

        final response = await client.getPetApi().addPet(
            pet: Pet((p) => p
              ..id = 5
              ..name = 'Paula'));

        expect(response.statusCode, 200);
      });
    });

    group('getMultiplePets', () {
      test('findByStatus', () async {
        tester.onRoute(
          '/pet/findByStatus',
          (server) => server.reply(200, [
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
              'status': Matchers.listParam<String>(
                ListParam(
                  ['available', 'sold'],
                  ListFormat.csv,
                ),
              ),
            },
          ),
        );

        final response = await client.getPetApi().findPetsByStatus(
              status: ListBuilder<String>(<String>[
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

    group('uploadFile', () {
      test('uploadFileWithRequiredFile', () async {
        final file = MultipartFile.fromBytes(
          [1, 2, 3, 4],
          filename: 'test.png',
          contentType: MediaType.parse('image/png'),
        );

        tester.onRoute(
          '/fake/5/uploadImageWithRequiredFile',
          (server) => server.reply(200, {
            'code': 200,
            'type': 'success',
            'message': 'File uploaded',
          }),
          request: Request(
            method: RequestMethods.post,
            headers: <String, dynamic>{
              Headers.contentTypeHeader:
                  Matchers.pattern('multipart/form-data'),
              Headers.contentLengthHeader: Matchers.integer,
            },
            data: Matchers.formData(
              FormData.fromMap(<String, dynamic>{
                r'requiredFile': file,
              }),
            ),
          ),
        );
        final response = await client.getPetApi().uploadFileWithRequiredFile(
              petId: 5,
              requiredFile: file,
            );

        expect(response.statusCode, 200);
        expect(response.data.message, 'File uploaded');
      });

      test('uploadFileWithRequiredFile & additionalMetadata', () async {
        final file = MultipartFile.fromBytes(
          [1, 2, 3, 4],
          filename: 'test.png',
          contentType: MediaType.parse('image/png'),
        );

        tester.onRoute(
          '/fake/3/uploadImageWithRequiredFile',
          (server) => server.reply(200, {
            'code': 200,
            'type': 'success',
            'message': 'File uploaded',
          }),
          request: Request(
            method: RequestMethods.post,
            headers: <String, dynamic>{
              Headers.contentTypeHeader:
                  Matchers.pattern('multipart/form-data'),
              Headers.contentLengthHeader: Matchers.integer,
            },
            data: Matchers.formData(
              FormData.fromMap(<String, dynamic>{
                'additionalMetadata': 'foo',
                r'requiredFile': file,
              }),
            ),
          ),
        );
        final response = await client.getPetApi().uploadFileWithRequiredFile(
              petId: 3,
              requiredFile: file,
              additionalMetadata: 'foo',
            );

        expect(response.statusCode, 200);
        expect(response.data.message, 'File uploaded');
      });
    });
  });
}
