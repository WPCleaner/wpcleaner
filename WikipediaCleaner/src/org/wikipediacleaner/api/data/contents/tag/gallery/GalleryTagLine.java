/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.tag.gallery;

import java.util.List;

import org.wikipediacleaner.api.data.contents.ContentsInterval;

/**
 * Bean representing a line of a gallery tag.
 */
public class GalleryTagLine extends ContentsInterval {

  private final String imageName;

  private final List<GalleryTagLineOption> options;

  public GalleryTagLine(
      int beginIndex, int endIndex,
      String imageName,
      List<GalleryTagLineOption> options) {
    super(beginIndex, endIndex);
    this.imageName = imageName;
    this.options = options;
  }

  public String getImageName() {
    return imageName;
  }

  public List<GalleryTagLineOption> getOptions() {
    return options;
  }
}
