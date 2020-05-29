/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.CharacterUtils;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 550 of check wikipedia project.
 * Error 550: Link without text.
 */
public class CheckErrorAlgorithm550 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm550() {
    super("Link without text");
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

    // Check each link
    boolean result = false;
    List<PageElementInternalLink> links = analysis.getInternalLinks();
    if (links == null) {
      return false;
    }
    PageElementInternalLink previousLink = null;
    for (PageElementInternalLink link : links) {

      // Analyze link
      String target = link.getFullLink();
      String text = link.getDisplayedTextNotTrimmed();
      boolean shouldReport = false;
      boolean automatic = true;
      boolean hasSpace = false;
      if ((target != null) && (!target.isEmpty())) {
        if ((text != null) && !text.isEmpty()) {
          shouldReport = true;
          int index = 0;
          while (shouldReport && (index < text.length())) {
            char currentChar = text.charAt(index);
            if (currentChar == '<') {
              PageElementTag tag = PageElementTag.analyzeBlock(text, index);
              if ((tag != null) &&
                  PageElementTag.TAG_WIKI_NOWIKI.equals(tag.getNormalizedName())) {
                index = tag.getEndIndex();
              } else {
                shouldReport = false;
              }
            } else if (CharacterUtils.isWhitespace(currentChar)) {
              index++;
              automatic = false;
              hasSpace = true;
            } else {
              shouldReport = false;
            }
          }
        }
      }
      if (shouldReport) {
        if (ignoreLinks.contains(link.getLink())) {
          shouldReport = false;
        }
      }

      // Report error
      if (shouldReport) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Check if modification can be automatic
        int beginIndex = link.getBeginIndex();
        int endIndex = link.getEndIndex();
        if (automatic) {
          String contents = analysis.getContents();
          if ((endIndex < contents.length()) && (contents.charAt(endIndex) == '\'')) {
            if ((endIndex < contents.length()) && (contents.charAt(endIndex) == '\'')) {
              automatic = false;
            }
            if ((previousLink != null) && (previousLink.getEndIndex() == beginIndex)) {
              automatic = false;
            }
          }
        }
        previousLink = link;

        // Report error
        CheckErrorResult errorResult = createCheckErrorResult(analysis, beginIndex, endIndex);
        errorResult.addReplacement("", automatic);
        if (hasSpace) {
          String contents = analysis.getContents();
          boolean otherSpace = false;
          if ((beginIndex > 0) &&
              CharacterUtils.isWhitespace(contents.charAt(beginIndex - 1))) {
            otherSpace = true;
          }
          if ((endIndex < contents.length()) &&
              CharacterUtils.isWhitespace(contents.charAt(endIndex))) {
            otherSpace = true;
          }
          if (!otherSpace) {
            errorResult.addReplacement(" ");
          }
        }
        errors.add(errorResult);
      }
    }

    return result;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    if (!analysis.getPage().isArticle() ||
        !analysis.getPage().isInMainNamespace()) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** List of links to be ignored */
  private static final String PARAMETER_IGNORE_LINKS = "ignore_links";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_IGNORE_LINKS, true, true, true);
    ignoreLinks.clear();
    if (tmp != null) {
      List<String> tmpList = WPCConfiguration.convertPropertyToStringList(tmp);
      if (tmpList != null) {
        ignoreLinks.addAll(tmpList);
      }
    }
  }

  /** Links to ignore */
  private final Set<String> ignoreLinks = new HashSet<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_IGNORE_LINKS,
        GT._T("Links to ignore"),
        new AlgorithmParameterElement(
            "article name",
            GT._T("Link to ignore")),
        true));
  }
}
