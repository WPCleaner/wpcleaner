/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageElementInternalLink;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ilink.ContentsInternalLinkBuilder;


/**
 * Algorithm for analyzing error 32 of check wikipedia project.
 * Error 32: Double pipe in one link.
 */
public class CheckErrorAlgorithm032 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm032() {
    super("Double pipe in one link");
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

    boolean result = false;
    Namespace fileNamespace = analysis.getWikiConfiguration().getNamespace(Namespace.IMAGE);
    String contents = analysis.getContents();
    for (PageElementInternalLink link : analysis.getInternalLinks()) {
      // Finding possible namespace
      String namespace = null;
      if (link.getLink() != null) {
        int colonPos = link.getLink().indexOf(':');
        if (colonPos > 0) {
          namespace = link.getLink().substring(0, colonPos).trim();
        }
      }

      // Analyze link text
      String text = link.getText();
      if ((text != null) &&
          ((namespace == null) || (!fileNamespace.isPossibleName(namespace)))) {
        int levelSquareBrackets = 0;
        int levelCurlyBrackets = 0;
        ArrayList<Integer> pipeIndex = new ArrayList<Integer>();
        int currentPos = 0;
        while (currentPos < text.length()) {
          switch (text.charAt(currentPos)) {
          case '[':
            // Checking if we have a [[
            if ((currentPos + 1 < text.length()) &&
                (text.charAt(currentPos + 1) == '[')) {
              levelSquareBrackets++;
              currentPos++;
            }
            break;
          case ']':
            // Checking if we have a ]]
            if ((currentPos + 1 < text.length()) &&
                (text.charAt(currentPos + 1) == ']')) {
              levelSquareBrackets--;
              currentPos++;
            }
            break;
          case '{':
            // Checking if we have a {{
            if ((currentPos + 1 < text.length()) &&
                (text.charAt(currentPos + 1) == '{')) {
              levelCurlyBrackets++;
              currentPos++;
            }
            break;
          case '}':
            // Checking if we have a }}
            if ((currentPos + 1 < text.length()) &&
                (text.charAt(currentPos + 1) == '}')) {
              levelCurlyBrackets--;
              currentPos++;
            }
            break;
          case '|':
            // Checking if the | is counting for
            if ((levelSquareBrackets == 0) &&
                (levelCurlyBrackets == 0)) {
              pipeIndex.add(Integer.valueOf(currentPos));
            }
            break;
          }
          currentPos++;
        }

        // Testing if the error has been found
        if ((levelSquareBrackets == 0) && (pipeIndex.size() > 0)) {
          if (errors == null) {
            return true;
          }
          result = true;

          // List replacements
          List<String> replacements = new ArrayList<String>();
          for (int i = 0; i <= pipeIndex.size(); i++) {
            int beginText = (i > 0) ? (pipeIndex.get(i - 1).intValue() + 1) : 0;
            int endText = (i < pipeIndex.size()) ? pipeIndex.get(i).intValue() : text.length();
            if ((beginText < endText) &&
                (text.substring(beginText, endText).trim().length() > 0)) {
              String replacement = PageElementInternalLink.createInternalLink(
                  link.getLink(), link.getAnchor(), text.substring(beginText, endText));
              if (!replacements.contains(replacement)) {
                replacements.add(replacement);
              }
            }
          }

          // Create error
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, link.getBeginIndex(), link.getEndIndex());
          boolean emptyLink = false;
          if ((link.getFullLink() == null) || (link.getFullLink().trim().length() == 0)) {
            errorResult.addReplacement(ContentsInternalLinkBuilder.from(link.getText()).toString());
            emptyLink = true;
          }
          for (String replacement : replacements) {
            errorResult.addReplacement(replacement, !emptyLink && (replacements.size() == 1));
          }
          errorResult.addReplacement("{{" + contents.substring(link.getBeginIndex() + 2, link.getEndIndex() - 2) + "}}");
          errors.add(errorResult);
        }
      }
    }
    return result;
  }

  /**
   * Automatic fixing of some errors in the page.
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
