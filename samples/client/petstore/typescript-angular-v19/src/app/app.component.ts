import {Component} from '@angular/core';
import {type Pet, PetService} from '@swagger/typescript-angular-petstore'
import {JsonPipe} from '@angular/common';

@Component({
  selector: 'app-root',
  imports: [JsonPipe],
  templateUrl: './app.component.html',
  styleUrl: './app.component.css'
})
export class AppComponent {
  title = 'typescript-angular-v19-unit-tests';
  pets: Pet[] | undefined;

  constructor(petService: PetService) {
    petService.findPetsByStatus(['available']).subscribe(pets => this.pets = pets);
  }
}
