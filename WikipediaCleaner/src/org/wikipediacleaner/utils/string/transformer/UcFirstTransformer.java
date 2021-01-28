/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.utils.string.transformer;

import org.wikipediacleaner.utils.string.CharacterUtils;

/**
 * A String transformer that put the first letter in upper case.
 */
public class UcFirstTransformer implements StringTransformer {

  /** Single instance */
  public static final UcFirstTransformer INSTANCE = new UcFirstTransformer();

  /**
   * Constructor.
   */
  private UcFirstTransformer() {
    // Nothing to do
  }

  /**
   * @param original Original String.
   * @return Transformed String with reduced whitespace characters.
   * @see org.wikipediacleaner.utils.string.transformer.StringTransformer#transform(java.lang.String)
   */
  @Override
  public String transform(String original) {
    return CharacterUtils.ucFirst(original);
  }

}
