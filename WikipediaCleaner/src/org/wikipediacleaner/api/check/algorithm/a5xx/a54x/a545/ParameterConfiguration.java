/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a54x.a545;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * Bean for handling configuration for template parameters.
 */
class ParameterConfiguration {

  @Nonnull private final String parameterName;

  @Nullable private String comment;

  @Nullable private String replacement;

  public ParameterConfiguration(String parameterName) {
    this.parameterName = parameterName;
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

  void setReplacement(@Nonnull String replacement) {
    this.replacement = replacement;
  }

  public @Nullable String getReplacement() {
    return replacement;
  }
}
