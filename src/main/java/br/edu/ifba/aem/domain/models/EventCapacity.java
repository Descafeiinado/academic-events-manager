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

  @Builder.Default
  public Long inPersonCapacity = 0L;
  @Builder.Default
  public Long virtualCapacity = 0L;

}
