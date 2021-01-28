/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.utils.string.transformer;

import java.util.regex.Pattern;

/**
 * A String transformer that reduces whitespace characters.
 */
public class ReduceWhitespaceTransformer implements StringTransformer {

  /** Single instance */
  public static final ReduceWhitespaceTransformer INSTANCE = new ReduceWhitespaceTransformer();

  /** Pattern to reduce whitespace characters */
  private final Pattern pattern = Pattern.compile("[\u00A0_ ]+");

  /**
   * Constructor.
   */
  private ReduceWhitespaceTransformer() {
    // Nothing to do
  }

  /**
   * @param original Original String.
   * @return Transformed String with reduced whitespace characters.
   * @see org.wikipediacleaner.utils.string.transformer.StringTransformer#transform(java.lang.String)
   */
  @Override
  public String transform(String original) {
    String result = pattern.matcher(original).replaceAll(" ").trim();
    if (!result.endsWith("\u200E")) {
      return result;
    }
    while (result.endsWith("\u200E")) {
      result = result.substring(0, result.length() - 1);
    }
    result = result.trim();
    return result;
  }

}
