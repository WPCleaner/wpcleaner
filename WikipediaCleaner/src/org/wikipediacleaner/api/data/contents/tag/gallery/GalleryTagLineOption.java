/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2024  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.tag.gallery;

import org.wikipediacleaner.api.data.contents.ContentsInterval;

/**
 * Bean representing an image option in a line of a gallery tag.
 */
public class GalleryTagLineOption extends ContentsInterval {

  private final String option;

  public GalleryTagLineOption(final int beginIndex, final int endIndex, final String option) {
    super(beginIndex, endIndex);
    this.option = option;
  }
  
  public String getOption() {
    return option;
  }
}
