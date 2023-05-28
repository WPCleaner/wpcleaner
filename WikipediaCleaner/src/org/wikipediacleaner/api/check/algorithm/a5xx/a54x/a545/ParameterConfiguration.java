/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a54x.a545;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.tuple.ImmutablePair;

/**
 * Bean for handling configuration for template parameters.
 */
class ParameterConfiguration {

  @Nonnull private final String parameterName;

  @Nullable private String comment;

  @Nullable private String nameReplacement;

  @Nonnull private Map<String, ImmutablePair<String, String>> replacementByValue;

  public ParameterConfiguration(String parameterName) {
    this.parameterName = parameterName;
    this.replacementByValue = new HashMap<>();
  }

  public @Nonnull String getParameterName() {
    return parameterName;
  }

  void setComment(@Nonnull String comment) {
    this.comment = comment;
  }

  public @Nullable String getComment() {
    return comment;
  }

  void setNameReplacement(@Nonnull String replacement) {
    this.nameReplacement = replacement;
  }

  public @Nullable String getNameReplacement() {
    return nameReplacement;
  }
  
  void addReplacementByValue(@Nonnull String value, @Nonnull String newName, @Nonnull String newValue) {
    replacementByValue.put(value, new ImmutablePair<String, String>(newName, newValue));
  }
  
  Optional<ImmutablePair<String, String>> getValueReplacement(@Nonnull String value) {
    return Optional.ofNullable(replacementByValue.get(value));
  }
}
