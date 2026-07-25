import { Component, inject } from '@angular/core'
import {
  PetService,
  StoreService,
  UserService,
  type Pet
} from '@swagger/typescript-angular-petstore'

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'Typescript Angular v22 (provided in root)'
  pet: Pet = {
      name: 'pet',
      photoUrls: []
    }
  store: Array<{ key: string, number: number }> = []

  private readonly petService = inject(PetService);
  private readonly storeService = inject(StoreService)
  private readonly userService = inject(UserService);

  public addPet (): void {
    this.petService.addPet(this.pet)
      .subscribe((result: Pet) => {
        this.pet = result
      })
  }

  public getPetByID (): void {
    if (this.pet.id !== undefined) {
      this.petService.getPetById(this.pet.id)
        .subscribe((result: Pet) => {
          this.pet = result
        })
    }
  }

  public updatePet (): void {
    this.petService.updatePet(this.pet)
      .subscribe((result: Pet) => {
        this.pet = result
      })
  }

  public deletePet (): void {
    if (this.pet.id !== undefined) {
      this.petService.deletePet(this.pet.id)
        .subscribe((result) => {
          this.pet = result
        })
    }
  }

  public getStoreInventory (): void {
    this.storeService.getInventory()
      .subscribe((result) => {
        this.store = []
        for (const item in result) {
          const number = result[item]
          this.store.push({ key: item, number })
        }
      })
  }
}
