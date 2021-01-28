/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.constants;

import javax.annotation.Nonnull;

import org.wikipediacleaner.utils.string.transformer.ListStringTransformer;
import org.wikipediacleaner.utils.string.transformer.ReduceWhitespaceTransformer;
import org.wikipediacleaner.utils.string.transformer.StringTransformer;
import org.wikipediacleaner.utils.string.transformer.UcFirstTransformer;


/**
 * Encapsulate possible rules for Case sensitiveness.
 */
public enum EnumCaseSensitiveness {

  FIRST_LETTER(
      "first-letter",
      new ListStringTransformer(
          ReduceWhitespaceTransformer.INSTANCE,
          UcFirstTransformer.INSTANCE)),
  CASE_SENSITIVE(
      "case-sensitive",
      new ListStringTransformer(ReduceWhitespaceTransformer.INSTANCE)),
  UNKNOWN(
      "",
      new ListStringTransformer(ReduceWhitespaceTransformer.INSTANCE));

  /**
   * Code representing the case sensitiveness.
   */
  @Nonnull
  private final String code;

  @Nonnull
  private final StringTransformer normalizer;

  /**
   * @param code Code representing the case sensitiveness.
   */
  private EnumCaseSensitiveness(
      @Nonnull String code,
      @Nonnull StringTransformer normalizer) {
    this.code = code;
    this.normalizer = normalizer;
  }

  /**
   * Retrieve value matching a given code.
   * 
   * @param code Code.
   * @return Value for the case sensitiveness.
   */
  public static EnumCaseSensitiveness getCase(String code) {
    if (FIRST_LETTER.code.equals(code)) {
      return FIRST_LETTER;
    }
    if (CASE_SENSITIVE.code.equals(code)) {
      return CASE_SENSITIVE;
    }
    return UNKNOWN;
  }

  /**
   * Normalize a text.
   * 
   * @param text Text to be normalized.
   * @return Normalized text.
   */
  public String normalize(String text) {
    return normalizer.transform(text);
  }
}
