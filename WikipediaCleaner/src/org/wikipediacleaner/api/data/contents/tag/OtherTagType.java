/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.tag;

import javax.annotation.Nonnull;

/**
 * Definition of various tag types.
 */
public class OtherTagType extends TagType {

  // Normalized names
  private static final String TYPO_NAME = "typo";

  // Normalized tags
  public static final OtherTagType TYPO = createRegularTag(TYPO_NAME);

  /**
   * Register tag types.
   */
  static void registerTagTypes() {
    // Do nothing, tags register by themselves
  }

  /**
   * Create a regular tag.
   * 
   * @param name Normalized name of the tag type.
   * @return Tag type.
   */
  private static OtherTagType createRegularTag(@Nonnull String name) {
    return new OtherTagType(name, true, true, true, false);
  }

  /**
   * Create a WIKI tag type.
   * 
   * @param name Normalized name of the tag type.
   * @param openPossible True if the tag can be open.
   * @param closePossible True if the tag can be close.
   * @param fullPossible True if the tag can be full.
   * @param unclosedOk True if an open tag can normally be without a close tag.
   */
  private OtherTagType(
      @Nonnull String name,
      boolean openPossible,
      boolean closePossible,
      boolean fullPossible,
      boolean unclosedOk) {
    super(name, openPossible, closePossible, fullPossible, unclosedOk);
  }
}
