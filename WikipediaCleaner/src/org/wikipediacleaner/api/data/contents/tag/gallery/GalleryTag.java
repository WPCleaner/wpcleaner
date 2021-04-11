/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.tag.gallery;

import java.util.Collections;
import java.util.List;

import javax.annotation.Nonnull;

/**
 * Bean representing a gallery tag.
 */
public class GalleryTag {

  @Nonnull
  private final List<GalleryTagLine> lines;

  GalleryTag(@Nonnull List<GalleryTagLine> lines) {
    this.lines = Collections.unmodifiableList(lines);
  }

  @Nonnull
  public List<GalleryTagLine> getLines() {
    return lines;
  }
}
