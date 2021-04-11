/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a03x.a035;

import java.util.Collection;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.api.data.contents.tag.gallery.GalleryTag;
import org.wikipediacleaner.api.data.contents.tag.gallery.GalleryTagAnalyzer;
import org.wikipediacleaner.api.data.contents.tag.gallery.GalleryTagLine;


/**
 * Algorithm for analyzing error 35 of check wikipedia project.
 * Error 35: Gallery image without description
 */
public class CheckErrorAlgorithm035 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm035() {
    super("Gallery image without description");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Analyze each gallery tag
    List<PageElementTag> galleryTags = analysis.getCompleteTags(WikiTagType.GALLERY);
    if (galleryTags.isEmpty()) {
      return false;
    }
    GalleryTagAnalyzer analyzer = new GalleryTagAnalyzer(getWikiConfiguration());
    String contents = analysis.getContents();
    boolean result = false;
    for (PageElementTag tag : galleryTags) {
      GalleryTag galleryTag = analyzer.analyze(tag, contents);
      for (GalleryTagLine line : galleryTag.getLines()) {
        if (StringUtils.isEmpty(line.getOptions())) {
          if (errors == null) {
            return true;
          }
          result = true;

          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, line.getBeginIndex(), line.getEndIndex());
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
