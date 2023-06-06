/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2023  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a52x.a529;

import java.util.Optional;

class AutomaticReplacement {

  public final String templateName;
  public final String prefix;
  public final String suffix;

  private AutomaticReplacement(final String templateName, final String prefix, final String suffix) {
    this.templateName = templateName;
    this.prefix = prefix;
    this.suffix = suffix;
  }

  public static Optional<AutomaticReplacement> build(final String[] params) {
    if ((params == null) || (params.length < 1)) {
      return Optional.empty();
    }
    return Optional.of(new AutomaticReplacement(
        params[0],
        params.length > 1 ? params[1] : "",
        params.length > 2 ? params[2] : ""));
  }
}
