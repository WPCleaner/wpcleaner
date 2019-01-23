/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 10 of check wikipedia project.
 * Error 10: Square brackets not correct end
 */
public class CheckErrorAlgorithm010 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm010() {
    super("Square brackets not correct end");
  }

  private final static String REJECTED_CHARS = "\n[{";

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

    // Analyze contents by looking for [[
    String contents = analysis.getContents();
    int maxLength = contents.length();
    int currentIndex = contents.indexOf("[[");
    boolean result = false;
    while (currentIndex >= 0) {
      boolean shouldCount = true;
      if (shouldCount) {
        PageElementInternalLink link = analysis.isInInternalLink(currentIndex);
        if ((link != null) && (link.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementImage image = analysis.isInImage(currentIndex);
        if ((image != null) && (image.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementCategory category = analysis.isInCategory(currentIndex);
        if ((category != null) && (category.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementLanguageLink link = analysis.isInLanguageLink(currentIndex);
        if ((link != null) && (link.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementInterwikiLink link = analysis.isInInterwikiLink(currentIndex);
        if ((link != null) && (link.getBeginIndex() == currentIndex)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementExternalLink link = analysis.isInExternalLink(currentIndex + 1);
        if ((link != null) && (link.getBeginIndex() == currentIndex + 1)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        if ((analysis.isInComment(currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_NOWIKI, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MAPFRAME, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_MATH_CHEM, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_PRE, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SCORE, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SOURCE, currentIndex) != null) ||
            (analysis.getSurroundingTag(PageElementTag.TAG_WIKI_SYNTAXHIGHLIGHT, currentIndex) != null) ||
            (analysis.isInTag(currentIndex) != null)) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        PageElementTemplate template = analysis.isInTemplate(currentIndex + 2);
        if ((template != null) && (contents.startsWith("]]", template.getEndIndex()))) {
          shouldCount = false;
        }
      }
      if (shouldCount) {
        if (errors == null) {
          return true;
        }
        result = true;
        
        // Check if there is a potential end
        int tmpIndex = currentIndex + 2;
        boolean errorReported = false;
        boolean finished = false;
        while (!finished && (tmpIndex < maxLength)) {
          char tmpChar = contents.charAt(tmpIndex);
          if (REJECTED_CHARS.indexOf(tmpChar) >= 0) {
            finished = true;
          } else if (tmpChar == ']') {
            int tmpIndex2 = tmpIndex + 1;
            while ((tmpIndex2 < maxLength) &&
                   (contents.charAt(tmpIndex2) != ']') &&
                   (REJECTED_CHARS.indexOf(contents.charAt(tmpIndex2)) < 0)) {
              tmpIndex2++;
            }
            String suffix = "";
            if ((tmpIndex2 < maxLength) && (contents.charAt(tmpIndex2) == ']')) {
              suffix = contents.substring(tmpIndex + 1, tmpIndex2 + 1);
            } else {
              tmpIndex2 = tmpIndex;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, currentIndex, tmpIndex2 + 1);

            // Check if the situation is something like [[http://....] (replacement: [http://....])
            boolean protocolFound = PageElementExternalLink.isPossibleProtocol(contents, currentIndex + 2);
            if (protocolFound) {
              errorResult.addReplacement(contents.substring(currentIndex + 1, tmpIndex2 + 1));
            }

            errorResult.addReplacement(contents.substring(currentIndex, tmpIndex + 1) + "]" + suffix);
            if (suffix.length() > 0) {
              errorResult.addReplacement(contents.substring(currentIndex, tmpIndex) + suffix + "]");
            }
            errors.add(errorResult);
            errorReported = true;
            finished = true;
          } else if (tmpChar == '}') {
            int lastChar = tmpIndex;
            if ((lastChar + 1 < maxLength) && (contents.charAt(lastChar + 1) == '}')) {
              lastChar++;
            }
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, currentIndex, lastChar + 1);
            errorResult.addReplacement(contents.substring(currentIndex, tmpIndex) + "]]");
            errorResult.addReplacement("{{" + contents.substring(currentIndex + 2, tmpIndex) + "}}");
            errors.add(errorResult);
            errorReported = true;
            finished = true;
          }
          tmpIndex++;
        }

        // Default
        if (!errorReported) {
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, currentIndex, currentIndex + 2);
          errorResult.addReplacement("", GT._T("Delete"));
          errors.add(errorResult);
        }
      }
      currentIndex = contents.indexOf("[[", currentIndex + 2);
    }

    // Analyze each internal link to see if it contains a [
    for (PageElementInternalLink link : analysis.getInternalLinks()) {
      String text = link.getText();
      if (text != null) {
        text = cleanText(text);
        if (text != null) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, link.getBeginIndex(), link.getEndIndex());
          errorResult.addReplacement(PageElementInternalLink.createInternalLink(
              link.getLink(), link.getAnchor(), text));
          errors.add(errorResult);
        }
      }
    }

    // Analyze each image to see if it contains a [
    for (PageElementImage image : analysis.getImages()) {
      String text = image.getDescription();
      String modifiedText = cleanText(text);
      String alt = image.getAlternateDescription();
      String modifiedAlt = cleanText(alt);
      if ((modifiedText != null) || (modifiedAlt != null)) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, image.getBeginIndex(), image.getEndIndex());
        errorResult.addReplacement(image.getDescriptionReplacement(
            (modifiedText != null) ? modifiedText : text,
            (modifiedAlt != null) ? modifiedAlt : alt));
        errors.add(errorResult);
      }
    }

    // Analyze each external link to see if it has a [ before
    for (PageElementExternalLink link : analysis.getExternalLinks()) {
      int begin = link.getBeginIndex();
      if (link.hasSquare()) {
        if ((begin > 0) && (contents.charAt(begin - 1) == '[')) {
          int end = link.getEndIndex();
          if ((end >= contents.length()) || (contents.charAt(end) != ']')) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, begin - 1, begin);
            errorResult.addReplacement("[");
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }

  /**
   * @param originalText Original text.
   * @return Cleaned up text (or null if no cleanup required).
   */
  private String cleanText(String originalText) {
    if (originalText == null) {
      return null;
    }
    StringBuilder sb = null;
    int index = 0;
    int singleBracketsCount = 0;
    while (index < originalText.length()) {
      boolean doubleBrackets = originalText.startsWith("[[", index);
      if (doubleBrackets || !originalText.startsWith("[", index)) {
        boolean  ok = true;
        if (originalText.startsWith("]", index)) {
          doubleBrackets = originalText.startsWith("]]", index);
          if (!doubleBrackets) {
            if (singleBracketsCount > 0) {
              singleBracketsCount--;
            } else {
              ok = false;
              if (sb == null) {
                sb = new StringBuilder(originalText.substring(0, index));
              }
            }
          }
        }
        int count = doubleBrackets ? 2 : 1;
        if (ok && (sb != null)) {
          sb.append(originalText.substring(index, index + count));
        }
        index += count;
      } else {
        singleBracketsCount++;
        boolean paired = false;
        int index2 = index + 1;
        while (!paired && (index2 < originalText.length())) {
          if (originalText.startsWith("]", index2) && !originalText.startsWith("]]", index2)) {
            paired = true;
          }
          index2++;
        }
        if (!paired) {
          if (sb == null) {
            sb = new StringBuilder(originalText.substring(0, index));
          }
        } else if (sb != null) {
          sb.append('[');
        }
        index++;
      }
    }
    return (sb != null) ? sb.toString() : null;
  }
}
