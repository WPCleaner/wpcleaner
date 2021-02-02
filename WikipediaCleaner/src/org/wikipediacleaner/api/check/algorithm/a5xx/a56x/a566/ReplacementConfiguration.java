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
import java.util.Optional;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Replacement;

/**
 * Bean for handling configuration for replacements.
 */
public class ReplacementConfiguration {

  /** Possible replacements depending on the value of the abbreviation */
  @Nonnull private final Map<String, InternalReplacement> replacementsByValue = new HashMap<>();

  /** Possible replacements depending on the value of the abbreviation and its title */
  @Nonnull private final Map<String, Map<String, InternalReplacement>> replacementByValueAndTitle = new HashMap<>();

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
   * Configure replacements by value.
   * 
   * @param rawConfiguration Raw configuration.
   */
  public void setReplacementsByValue(
      @Nullable List<String[]> rawConfiguration) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] rawLine : rawConfiguration) {
      if ((rawLine.length >= 3) && StringUtils.isNotEmpty(rawLine[0])) {
        replacementsByValue.put(rawLine[0], new InternalReplacement(rawLine[2], Boolean.parseBoolean(rawLine[1])));
      }
    }
  }

  /**
   * Configure replacements by value and title.
   * 
   * @param rawConfiguration Raw configuration.
   */
  public void setReplacementsByValueAndTitle(
      @Nullable List<String[]> rawConfiguration) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] rawLine : rawConfiguration) {
      if ((rawLine.length >= 4) && StringUtils.isNotEmpty(rawLine[0])) {
        Map<String, InternalReplacement> valueMap = replacementByValueAndTitle.computeIfAbsent(rawLine[0], k -> new HashMap<>());
        valueMap.put(rawLine[1], new InternalReplacement(rawLine[3], Boolean.parseBoolean(rawLine[2])));
      }
    }
  }

  /**
   * Get a replacement.
   * 
   * @param errorResult Error result for which the replacement should be added.
   * @param abbreviation Abbreviation.
   * @param title Optional title.
   * @return Replacement.
   */
  public Optional<Replacement> getReplacement(
      @Nonnull CheckErrorResult errorResult,
      String abbreviation,
      @Nullable String title) {
    boolean acceptAutomatic = true;
    InternalReplacement replacement = null;
    if (StringUtils.isNotEmpty(title)) {
      Map<String, InternalReplacement> mapValue = replacementByValueAndTitle.get(abbreviation);
      if (mapValue != null) {
        replacement = mapValue.get(title.trim());
      }
    }
    if (replacement == null) {
      replacement = replacementsByValue.get(abbreviation);
      acceptAutomatic = StringUtils.isEmpty(title);
    }
    if (replacement == null) {
      return Optional.empty();
    }
    return Optional.of(new Replacement(
        replacement.replacement,
        replacement.automatic && acceptAutomatic));
  }

  /**
   * Bean for handling configuration for one replacement.
   */
  private static final class InternalReplacement {
    final boolean automatic;
    final String replacement;
    
    public InternalReplacement(String replacement, boolean automatic) {
      this.replacement = replacement;
      this.automatic = automatic;
    }
  }
}
