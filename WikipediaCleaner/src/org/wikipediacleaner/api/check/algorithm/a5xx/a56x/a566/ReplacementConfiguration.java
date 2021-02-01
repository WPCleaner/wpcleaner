/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a566;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.CheckErrorResult;

/**
 * Bean for handling configuration for replacements.
 */
public class ReplacementConfiguration {

  /** Possible replacements */
  @Nonnull private final Map<String, Replacement> replacementsByValue = new HashMap<>();

  /**
   * Public constructor.
   */
  public ReplacementConfiguration() {
  }

  /**
   * Clear configuration.
   */
  public void clearConfiguration() {
    this.replacementsByValue.clear();
  }

  /**
   * Initialize configuration.
   * 
   * @param rawConfiguration Raw configuration.
   */
  public void initializeConfiguration(
      @Nullable List<String[]> rawConfiguration) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] rawLine : rawConfiguration) {
      if ((rawLine.length >= 3) && StringUtils.isNotEmpty(rawLine[0])) {
        replacementsByValue.put(rawLine[0], new Replacement(rawLine[2], Boolean.parseBoolean(rawLine[1])));
      }
    }
  }

  /**
   * Add a replacement.
   * 
   * @param errorResult Error result for which the replacement should be added.
   * @param abbreviation Abbreviation.
   * @param title Optional title.
   */
  public void addReplacement(@Nonnull CheckErrorResult errorResult, String abbreviation, @Nullable String title) {
    Replacement replacement = replacementsByValue.get(abbreviation);
    if (replacement == null) {
      return;
    }
    errorResult.addReplacement(
        replacement.replacement,
        replacement.automatic && StringUtils.isEmpty(title));
  }

  /**
   * Bean for handling configuration for one replacement.
   */
  private static final class Replacement {
    final boolean automatic;
    final String replacement;
    
    public Replacement(String replacement, boolean automatic) {
      this.replacement = replacement;
      this.automatic = automatic;
    }
  }
}
