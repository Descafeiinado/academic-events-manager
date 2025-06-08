package br.edu.ifba.aem.domain.models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@Builder
@Data
@NoArgsConstructor
public class EventCapacity {

  public Long inPersonCapacity = 0L;
  public Long virtualCapacity = 0L;

}
