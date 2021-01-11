/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


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

    // Retrieve image name space
    Namespace imageNamespace = analysis.getWikiConfiguration().getNamespace(Namespace.IMAGE);

    // Analyze each gallery tag
    List<PageElementTag> galleryTags = analysis.getCompleteTags(WikiTagType.GALLERY);
    String contents = analysis.getContents();
    boolean result = false;
    for (PageElementTag galleryTag : galleryTags) {
      if (galleryTag.getMatchingTag() != null) {
        PageElementTag endTag = galleryTag.getMatchingTag();
        int beginIndex = galleryTag.getEndIndex();
        int tmpIndex = beginIndex;
        while (tmpIndex <= endTag.getBeginIndex()) {
          if ((tmpIndex == endTag.getBeginIndex()) ||
              (contents.charAt(tmpIndex) == '\n')) {
            String line = contents.substring(beginIndex, tmpIndex).trim();
            int colonIndex = line.indexOf(':');
            if ((colonIndex > 0) && (imageNamespace.isPossibleName(line.substring(0, colonIndex)))) {
              int pipeIndex = line.indexOf('|', colonIndex);
              boolean description = false;
              if ((pipeIndex >= 0) && (pipeIndex + 1 < line.length())) {
                if (line.substring(pipeIndex + 1).trim().length() > 0) {
                  description = true;
                }
              }
              if (!description) {
                if (errors == null) {
                  return true;
                }
                result = true;

                CheckErrorResult errorResult = createCheckErrorResult(
                    analysis, beginIndex, tmpIndex);
                errors.add(errorResult);
              }
            }
            beginIndex = tmpIndex + 1;
          }
          tmpIndex++;
        }
      }
    }
    return result;
  }
}
