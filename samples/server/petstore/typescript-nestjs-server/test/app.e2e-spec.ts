import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import * as request from 'supertest';
import { App } from 'supertest/types';
import { AppModule } from '../src/app.module';
import { PetApiController } from '../builds/default/controllers';

describe('AppModule (e2e)', () => {
  let app: INestApplication<App>;

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();

    app = moduleFixture.createNestApplication();
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
        .expect({name: 'MyPetB', photoUrls: []});
    });

    it('should handle POST request with body', () => {
      return request(app.getHttpServer())
        .post('/pet')
        .send({name: 'POST Pet'})
        .expect(201)
        .expect({name: 'POST Pet'});
    });

    it('should handle GET request with query params', () => {
      return request(app.getHttpServer())
        .get('/pet/findByStatus')
        .query({status: ['available', 'pending']})
        .expect(200)
        .expect([{name: 'available', photoUrls: []}, {name: 'pending', photoUrls: []}]);
    });
  })


});
