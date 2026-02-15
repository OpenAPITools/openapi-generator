import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import * as request from 'supertest';
import { App } from 'supertest/types';
import { AppModule } from '../src/app.module';
import { PetApiController } from '../builds/default/controllers';
import { DefaultService } from '../src/handlers/parameters/DefaultService';
import { DefaultApi } from '../builds/parameters/api';
import * as cookieParser from 'cookie-parser';

describe('AppModule (e2e)', () => {
  let app: INestApplication<App>;

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();

    app = moduleFixture.createNestApplication();
    app.use(cookieParser());
    await app.init();
  });

  describe('PetApiController', () => {
    it('should be provided', () => {
      const petApiController = app.get<PetApiController>(PetApiController);
      expect(petApiController).toBeTruthy();
    });

    it('should handle GET request with path param /pet/{id}', () => {
      return request(app.getHttpServer())
        .get('/pet/1')
        .expect(200)
        .expect({ name: 'MyPetB', photoUrls: [] });
    });

    it('should handle POST request with body', () => {
      return request(app.getHttpServer())
        .post('/pet')
        .send({ name: 'POST Pet' })
        .expect(201)
        .expect({ name: 'POST Pet' });
    });

    it('should handle GET request with query params', () => {
      return request(app.getHttpServer())
        .get('/pet/findByStatus')
        .query({ status: ['available', 'pending'] })
        .expect(200)
        .expect([
          { name: 'available', photoUrls: [] },
          { name: 'pending', photoUrls: [] },
        ]);
    });
  });

  describe('ParametersDefaultController', () => {
    it('should set default parameters', () => {
      let defaultService: DefaultService = app.get(DefaultApi);

      return request(app.getHttpServer())
        .get('/test/parameters/path_a/path_b')
        .expect((res)=> {
          console.log('Body:', JSON.stringify(res.body, null, 2));
        })
        .expect(200)
        .then(() => {
          expect(defaultService.lastRequestParams).toEqual({
            pathDefault: 'path_a',
            pathNullable: 'path_b',
            queryDefault: 'available',
            queryDefaultEnum: 'B',
            queryDefaultInt: 3,
            headerDefault: 'available',
            headerDefaultEnum: 'B',
            headerDefaultInt: 3,
            cookieDefault: 'available',
            cookieDefaultEnum: 'B',
            cookieDefaultInt: 3,
            queryNullable: undefined,
            headerNullable: undefined,
            cookieNullable: undefined,
            $query$dollarSign: undefined,
          });
        });
    });


    it('should receive request parameters', () => {
      let defaultService: DefaultService = app.get(DefaultApi);

      return request(app.getHttpServer())
        .get('/test/parameters/path_a/path_b')
        .query({
          query_default: 'custom_query',
          query_default_enum: 'C',
          query_default_int: 5,
          query_nullable: 'not_null_query',
          '$query-$dollar-sign': '$$$',
        })
        .set('header_default', 'custom_header')
        .set('header_default_enum', 'C')
        .set('header_default_int', '6')
        .set('header_nullable', 'null')
        .set(
          'Cookie',
          'cookie_default=custom_cookie; cookie_default_enum=C; cookie_default_int=7; cookie_nullable=a_cookie',
        )
        .expect(200)
        .then(() => {
          expect(defaultService.lastRequestParams).toEqual({
            pathDefault: 'path_a',
            pathNullable: 'path_b',
            queryDefault: 'custom_query',
            queryDefaultEnum: 'C',
            queryDefaultInt: 5,
            headerDefault: 'custom_header',
            headerDefaultEnum: 'C',
            headerDefaultInt: 6,
            cookieDefault: 'custom_cookie',
            cookieDefaultEnum: 'C',
            cookieDefaultInt: 7,
            queryNullable: 'not_null_query',
            headerNullable: 'null',
            cookieNullable: 'a_cookie',
            $query$dollarSign: '$$$',
          });
        });
    });
  });
});
