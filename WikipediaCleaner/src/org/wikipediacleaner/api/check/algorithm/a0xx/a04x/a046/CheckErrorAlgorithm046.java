/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a0xx.a04x.a046;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementCategory;
import org.wikipediacleaner.api.data.PageElementExternalLink;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementInterwikiLink;
import org.wikipediacleaner.api.data.PageElementLanguageLink;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 46 of check wikipedia project.
 * Error 46: Square brackets not correct begin
 */
public class CheckErrorAlgorithm046 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm046() {
    super("Square brackets not correct begin");
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

    // Analyze contents from the beginning
    String contents = analysis.getContents();
    int currentIndex = contents.indexOf("]]");
    boolean result = false;
    while (currentIndex > 0) {
      result |= analyzeIndex(analysis, errors, currentIndex);
      currentIndex = contents.indexOf("]]", currentIndex + 2);
    }

    return result;
  }

  /**
   * Analyze a page to check if errors are present at a given index.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param currentIndex Index to analyze.
   * @return Flag indicating if the error was found.
   */
  private boolean analyzeIndex(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      int currentIndex) {
    
    // Ignore some situations
    PageElementInternalLink iLink = analysis.isInInternalLink(currentIndex);
    if ((iLink != null) &&
        ((iLink.getEndIndex() == currentIndex + 2) ||
         (iLink.getEndIndex() == currentIndex + 3))) {
      return false;
    }
    PageElementImage image = analysis.isInImage(currentIndex);
    if ((image != null) &&
        ((image.getEndIndex() == currentIndex + 2) ||
         (image.getEndIndex() == currentIndex + 3))) {
      return false;
    }
    PageElementCategory category = analysis.isInCategory(currentIndex);
    if ((category != null) && (category.getEndIndex() == currentIndex + 2)) {
      return false;
    }
    PageElementLanguageLink langLink = analysis.isInLanguageLink(currentIndex);
    if ((langLink != null) && (langLink.getEndIndex() == currentIndex + 2)) {
      return false;
    }
    PageElementInterwikiLink interLink = analysis.isInInterwikiLink(currentIndex);
    if ((interLink != null) && (interLink.getEndIndex() == currentIndex + 2)) {
      return false;
    }
    if (analysis.comments().isAt(currentIndex) ||
        (analysis.getSurroundingTag(WikiTagType.NOWIKI, currentIndex) != null) ||
        (analysis.getSurroundingTag(HtmlTagType.CODE, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.MAPFRAME, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.MATH, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.MATH_CHEM, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.PRE, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.SCORE, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.SOURCE, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.SYNTAXHIGHLIGHT, currentIndex) != null) ||
        (analysis.isInTag(currentIndex) != null)) {
      return false;
    }
    String contents = analysis.getContents();
    PageElementTemplate template = analysis.isInTemplate(currentIndex - 1);
    if ((template != null) &&
        (template.getEndIndex() == currentIndex) &&
        (contents.startsWith("[[", template.getBeginIndex() - 2))) {
      return false;
    }

    // Handle specific case inside external link
    PageElementExternalLink eLink = analysis.isInExternalLink(currentIndex);
    if ((eLink != null) &&
        (eLink.getEndIndex() == currentIndex + 1)) {
      if ((eLink.getBeginIndex() != 0) &&
          (contents.charAt(eLink.getBeginIndex() - 1) == '[')) {
        return false;
      }
      if (errors == null) {
        return true;
      }

      boolean automatic = true;
      if (contents.substring(eLink.getBeginIndex() + 1, currentIndex).indexOf('[') >= 0) {
        automatic = false;
      }
      if (automatic) {
        int tmpIndex = eLink.getBeginIndex();
        while ((tmpIndex > 0) && (contents.charAt(tmpIndex - 1) != '\n')) {
          tmpIndex--;
          if (contents.charAt(tmpIndex) == '[') {
            PageElementExternalLink eLinkTmp = analysis.isInExternalLink(tmpIndex);
            PageElementInternalLink iLinkTmp = analysis.isInInternalLink(tmpIndex);
            if ((eLinkTmp == null) && (iLinkTmp == null)) {
              automatic = false;
            }
          }
        }
      }
      CheckErrorResult errorResult = createCheckErrorResult(
          analysis, currentIndex, currentIndex + 2);
      errorResult.addReplacement("]", automatic);
      errors.add(errorResult);
      return true;
    }

    // Report error
    if (errors == null) {
      return true;
    }

    // Check if there is a potential beginning
    int tmpIndex = currentIndex - 1;
    boolean errorReported = false;
    boolean finished = false;
    while (!finished && tmpIndex >= 0) {
      char tmpChar = contents.charAt(tmpIndex);
      if ((tmpChar == '\n') ||
          (tmpChar == ']') ||
          (tmpChar == '}')) {
        finished = true;
      } else if (tmpChar == '[') {
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, tmpIndex, currentIndex + 2);

        // Check if the situation is something like [http://....]] (replacement: [http://....])
        boolean protocolFound = PageElementExternalLink.isPossibleProtocol(contents, tmpIndex + 1);
        if (protocolFound) {
          errorResult.addReplacement(contents.substring(tmpIndex, currentIndex + 1));
        }

        errorResult.addReplacement("[" + contents.substring(tmpIndex, currentIndex + 2));
        errors.add(errorResult);
        errorReported = true;
        finished = true;
      } else if (tmpChar == '{') {
        int firstChar = tmpIndex;
        if ((firstChar > 0) && (contents.charAt(firstChar - 1) == '{')) {
          firstChar--;
        }
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, firstChar, currentIndex + 2);
        errorResult.addReplacement("[[" + contents.substring(tmpIndex + 1, currentIndex + 2));
        errorResult.addReplacement("{{" + contents.substring(tmpIndex + 1, currentIndex) + "}}");
        errors.add(errorResult);
        errorReported = true;
        finished = true;
      }
      tmpIndex--;
    }
    if (errorReported) {
      return true;
    }

    // Default
    boolean automatic = false;
    if ((currentIndex >= 2) && (contents.startsWith("]]", currentIndex - 2))) {
      PageElementCategory tmpCategory = analysis.isInCategory(currentIndex - 2);
      if ((tmpCategory != null) && (tmpCategory.getEndIndex() == currentIndex)) {
        automatic = true;
      }
      PageElementInternalLink link = analysis.isInInternalLink(currentIndex - 2);
      if ((link != null) && (link.getEndIndex() == currentIndex)) {
        automatic = true;
      }
    }
    if (analysis.getSurroundingTag(WikiTagType.GALLERY, currentIndex) != null) {
      if (contents.startsWith("\n", currentIndex + 2)) {
        automatic = true;
      }
    }
    automatic &= !hasUnmatchedOpeningBracketsBefore(analysis, contents, currentIndex);
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, currentIndex, currentIndex + 2);
    errorResult.addReplacement("", GT._T("Delete"), automatic);
    errors.add(errorResult);

    return true;
  }

  private boolean hasUnmatchedOpeningBracketsBefore(PageAnalysis analysis, String contents, int limit) {
    int currentIndex = contents.indexOf("[[");
    while ((currentIndex > 0) && (currentIndex < limit)) {
      if (isUnmatchedOpeningBrackets(analysis, currentIndex)) {
        return true;
      }
      currentIndex = contents.indexOf("[[", currentIndex + 1);
    }
    return false;
  }

  private boolean isUnmatchedOpeningBrackets(PageAnalysis analysis, int currentIndex) {
    PageElementInternalLink iLink = analysis.isInInternalLink(currentIndex);
    if ((iLink != null) && (iLink.getBeginIndex() == currentIndex)) {
      return true;
    }

    PageElementImage image = analysis.isInImage(currentIndex);
    if ((image != null) && (image.getBeginIndex() == currentIndex)) {
      return true;
    }
    
    PageElementCategory category = analysis.isInCategory(currentIndex);
    if ((category != null) && (category.getBeginIndex() == currentIndex)) {
      return true;
    }
    
    PageElementLanguageLink lLink = analysis.isInLanguageLink(currentIndex);
    if ((lLink != null) && (lLink.getBeginIndex() == currentIndex)) {
      return true;
    }
    
    PageElementInterwikiLink iwLink = analysis.isInInterwikiLink(currentIndex);
    if ((iwLink != null) && (iwLink.getBeginIndex() == currentIndex)) {
      return true;
    }
    
    return false;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }
}
