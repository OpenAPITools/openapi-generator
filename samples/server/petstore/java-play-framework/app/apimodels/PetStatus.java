package apimodels;

import java.util.Objects;
import javax.validation.constraints.*;
import com.fasterxml.jackson.annotation.*;
import com.fasterxml.jackson.annotation.JsonCreator;

/**
 * Pet's status
 */
public enum PetStatus {
  
  HEALTHY("Healthy"),
  
  SICK("Sick"),
  
  QUARANTINED("Quarantined"),
  
  INPETSHEAVEN("InPetsHeaven");

  private String value;

  PetStatus(String value) {
    this.value = value;
  }

  @Override
  @JsonValue
  public String toString() {
    return String.valueOf(value);
  }

  @JsonCreator
  public static PetStatus fromValue(String text) {
    for (PetStatus b : PetStatus.values()) {
      if (String.valueOf(b.value).equals(text)) {
        return b;
      }
    }
    return null;
  }
}

