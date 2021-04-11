/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.tag.gallery;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.data.contents.ContentsInterval;

/**
 * Bean representing a line of a gallery tag.
 */
public class GalleryTagLine extends ContentsInterval {

  private final String imageName;

  private final String options;

  public GalleryTagLine(
      int beginIndex, int endIndex,
      String imageName, String options) {
    super(beginIndex, endIndex);
    this.imageName = imageName;
    this.options = StringUtils.defaultString(options);
  }

  public String getImageName() {
    return imageName;
  }

  public String getOptions() {
    return options;
  }
}
