import {Component} from '@angular/core';
import {type Pet, PetService} from '@swagger/typescript-angular-petstore'
import {JsonPipe} from '@angular/common';
import { Car, DefaultService } from '@swagger/typescript-angular-deepobject';

@Component({
  selector: 'app-root',
  imports: [JsonPipe],
  templateUrl: './app.component.html',
  styleUrl: './app.component.css'
})
export class AppComponent {
  title = 'typescript-angular-v19-unit-tests';
  pets: Pet[] | undefined;
  cars: Car[] = [];

  constructor(petService: PetService, defaultService: DefaultService) {
    petService.findPetsByStatus(['available']).subscribe(pets => this.pets = pets);
    defaultService.getCars({ make: 'bmw', model: '319' }).subscribe(result => this.cars = result);
  }
}
