import { Component } from '@angular/core';
import {
  PetService,
  StoreService,
  UserService,
  Pet
} from '@swagger/typescript-angular-petstore';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'Typescript Angular v11 (provided in root)';
  pet: Pet;
  store: { key: string, number: number }[];

  constructor(private petService: PetService,
              private storeService: StoreService,
              private userService: UserService,
              ) {
    this.pet = {
      name: `pet`,
      photoUrls: []
    };
  }

  public addPet() {
    this.petService.addPet(this.pet)
      .subscribe((result) => {
        this.pet = result;
      }
    );
  }

  public getPetByID() {
    this.petService.getPetById(this.pet.id)
      .subscribe((result) => {
        this.pet = result;
      }
    );
  }

  public updatePet() {
    this.petService.updatePet(this.pet)
      .subscribe((result) => {
        this.pet = result;
      }
    );
  }

  public deletePet() {
    this.petService.deletePet(this.pet.id)
      .subscribe((result) => {
        this.pet = result;
      }
    );
  }

  public getStoreInventory() {
    this.storeService.getInventory()
      .subscribe((result) => {
        this.store = [];
        for(let item in result) {
          const number = result[item];
          this.store.push({ key: item, number: number});
        }
      })
  }
}
