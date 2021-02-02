/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.check.algorithm.a5xx.a56x.a566;

import java.util.List;
import java.util.Optional;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Replacement;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;

/**
 * Bean for handling configuration for templates.
 */
public class TemplateConfiguration {

  @Nonnull private final String templateName;
  private final String paramAbbreviation;
  private final String paramMeaning;
  @Nonnull private final String defaultMeaning;

  /**
   * Constructor.
   * 
   * @param templateName Name of the template.
   * @param paramAbbreviation Name of the parameter for the abbreviation.
   * @param paramMeaning Name of the parameter for the meaning.
   * @param defaultMeaning Default value for the meaning.
   */
  private TemplateConfiguration(
      @Nonnull final String templateName,
      final String paramAbbreviation,
      final String paramMeaning,
      @Nonnull final String defaultMeaning) {
    this.templateName = templateName;
    this.paramAbbreviation = paramAbbreviation;
    this.paramMeaning = paramMeaning;
    this.defaultMeaning = defaultMeaning;
  }

  /**
   * Initialize configuration.
   * 
   * @param configuration Configuration to be initialized.
   * @param rawConfiguration Raw configuration.
   */
  public static void initializeConfiguration(
      List<TemplateConfiguration> configuration,
      List<String[]> rawConfiguration) {
    if (rawConfiguration == null) {
      return;
    }
    for (String[] rawLine : rawConfiguration) {
      if ((rawLine.length >= 3) && StringUtils.isNotEmpty(rawLine[0])) {
        configuration.add(new TemplateConfiguration(
            rawLine[0], rawLine[1], rawLine[2],
            rawLine.length > 3 ? rawLine[3] : StringUtils.EMPTY));
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
  public Optional<Replacement> getReplacement(@Nonnull CheckErrorResult errorResult, String abbreviation, @Nullable String title) {
    String replacement = TemplateBuilder
        .from(templateName)
        .addParam(paramAbbreviation, abbreviation)
        .addParam(paramMeaning, StringUtils.defaultString(title, defaultMeaning).trim())
        .removeUnnecessaryParamNames(true)
        .toString();
    return Optional.of(new Replacement(replacement, false));
  }
}
