/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 80 of check wikipedia project.
 * Error 80: External link with line break.
 */
public class CheckErrorAlgorithm080 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm080() {
    super("External link with line break");
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

    // Check each external link
    Collection<PageElementExternalLink> links = analysis.getExternalLinks();
    String contents = analysis.getContents();
    int maxLength = contents.length();
    boolean result = false;
    for (PageElementExternalLink link : links) {
      int linkBeginIndex = link.getBeginIndex();
      int beginIndex = linkBeginIndex - 1;
      if (!link.hasSquare() &&
          !link.hasSecondSquare() &&
          (linkBeginIndex > 0) &&
          (contents.charAt(beginIndex) == '[')) {

        // Compute maximum index for link end
        int linkEndIndex = link.getEndIndex();
        int maxEnd = maxLength;
        PageElementTag refTag = analysis.getSurroundingTag(WikiTagType.REF, linkBeginIndex);
        if ((refTag != null) &&
            (refTag.getMatchingTag() != null) &&
            (refTag.getMatchingTag().getBeginIndex() >= linkEndIndex)) {
          maxEnd = refTag.getMatchingTag().getBeginIndex();
        }

        // Search for possible end
        boolean searchDone = false;
        int possibleEnd = -1;
        int currentIndex = linkEndIndex;
        int firstCrIndex = -1;
        while (!searchDone &&
               (currentIndex < maxEnd) &&
               (contents.charAt(currentIndex) != ']')) {
          boolean posDone = false;

          // Check for carriage return
          if ((firstCrIndex < 0) && (contents.charAt(currentIndex) == '\n')) {
            firstCrIndex = currentIndex;
          }

          // Check for an other external link
          if (!posDone) {
            PageElementExternalLink externalLink = analysis.isInExternalLink(currentIndex);
            if (externalLink != null) {
              possibleEnd = currentIndex;
              searchDone = true;
              posDone = true;
            }
          }

          if (!posDone) {
            currentIndex++;
          }
        }
        if ((possibleEnd < 0) &&
            (currentIndex < maxEnd) &&
            (contents.charAt(currentIndex) == ']')) {
          possibleEnd = currentIndex + 1;
        }
        if ((possibleEnd < 0) &&
            (currentIndex == maxEnd) &&
            (maxEnd < maxLength)) {
          possibleEnd = currentIndex;
        }

        if ((possibleEnd > 0) || (firstCrIndex > 0)) {
          while ((possibleEnd > 0) &&
                 Character.isWhitespace(contents.charAt(possibleEnd - 1))) {
            possibleEnd--;
          }
          while ((firstCrIndex > 0) &&
                 Character.isWhitespace(contents.charAt(firstCrIndex - 1))) {
            firstCrIndex--;
          }
          if ((possibleEnd >= link.getEndIndex()) || (firstCrIndex >= link.getEndIndex())) {
            if (errors == null) {
              return true;
            }
            result = true;
            int endIndex = Math.max(possibleEnd, firstCrIndex);
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, beginIndex, endIndex);
            if (possibleEnd > 0) {
              StringBuilder replacement = new StringBuilder();
              replacement.append(contents.substring(beginIndex, linkEndIndex));
              if (linkEndIndex < possibleEnd) {
                String tmp = contents.substring(linkEndIndex, possibleEnd).replaceAll("\\n", "");
                if ((tmp.length() > 0) && !Character.isWhitespace(tmp.charAt(0))) {
                  replacement.append(' ');
                }
                replacement.append(tmp);
              }
              if (contents.charAt(possibleEnd - 1) != ']') {
                replacement.append("]");
              }
              replacement.append(contents.substring(possibleEnd, endIndex));
              errorResult.addReplacement(replacement.toString());
            }
            if (firstCrIndex > 0) {
              errorResult.addReplacement(
                  contents.substring(beginIndex, firstCrIndex) + "]" +
                  contents.substring(firstCrIndex, endIndex));
            }
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }
}
