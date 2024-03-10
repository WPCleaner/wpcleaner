/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2021  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.tag.gallery;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Nonnull;

import org.wikipediacleaner.api.configuration.WikiConfiguration;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementTag;

/**
 * Analyzer of gallery tags.
 */
public class GalleryTagAnalyzer {

  private final Namespace imageNamespace;
  private final String contents;

  /**
   * @param config Configuration.
   */
  public GalleryTagAnalyzer(@Nonnull WikiConfiguration config, @Nonnull String contents) {
    imageNamespace = config.getNamespace(Namespace.IMAGE);
    this.contents = contents;
  }

  public GalleryTag analyze(PageElementTag tag) {
    int beginIndex = tag.getValueBeginIndex();
    int endIndex = tag.getValueEndIndex();
    List<GalleryTagLine> lines = new ArrayList<>();
    while (beginIndex < endIndex) {
      beginIndex = addLine(beginIndex, endIndex, lines);
    }
    return new GalleryTag(lines);
  }

  private int addLine(
      int beginIndex, int endIndex,
      List<GalleryTagLine> lines) {

    // Find image name
    int tmpIndex = beginIndex;
    while ((tmpIndex < endIndex) &&
           (contents.charAt(tmpIndex) != '\n')) {
      tmpIndex++;
    }
    int endNameIndex = contents.indexOf('|', beginIndex);
    if ((endNameIndex > tmpIndex) || (endNameIndex < 0)) {
      endNameIndex = tmpIndex;
    }
    String imageName = contents.substring(beginIndex, endNameIndex);
    int dotIndex = imageName.lastIndexOf('.');
    if (dotIndex <= 0) {
      return tmpIndex + 1;
    }
    int colonIndex = imageName.indexOf(':');
    if ((colonIndex > 0) && !imageNamespace.isPossibleName(contents.substring(beginIndex, beginIndex + colonIndex))) {
      return tmpIndex + 1;
    }

    // Report line without options
    if (endNameIndex >= tmpIndex) {
      lines.add(new GalleryTagLine(beginIndex, tmpIndex, imageName, null));
      return tmpIndex + 1;
    }

    // TODO: analyze options
    lines.add(new GalleryTagLine(beginIndex, tmpIndex, imageName, contents.substring(endNameIndex + 1, tmpIndex)));
    return tmpIndex + 1;
  }
}
