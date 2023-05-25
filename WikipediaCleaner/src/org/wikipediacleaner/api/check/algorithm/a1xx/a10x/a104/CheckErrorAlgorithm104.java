/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a1xx.a10x.a104;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTag.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.HtmlTagType;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;


/**
 * Algorithm for analyzing error 104 of check wikipedia project.
 * Error 104: Unbalanced quotes in ref name
 */
public class CheckErrorAlgorithm104 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm104() {
    super("Unbalanced quotes in ref name");
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

    // Check every "<"
    boolean result = false;
    String contents = analysis.getContents();
    int currentIndex = contents.indexOf('<');
    while (currentIndex >= 0) {
      result |= analyzeIndex(analysis, errors, currentIndex);
      currentIndex = contents.indexOf('<', currentIndex + 1);
    }

    return result;
  }

  /**
   * Analyze a position in the page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param currentIndex Current index in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeIndex(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors,
      int currentIndex) {

    // Ignore tags correctly detected
    PageElementTag tag = analysis.isInTag(currentIndex);
    if ((tag != null) && (tag.getBeginIndex() == currentIndex)) {
      if (!WikiTagType.REF.equals(tag.getType())) {
        return false;
      }
      boolean ok = true;
      Parameter paramName = tag.getParameter("name");
      if ((paramName != null) && paramName.hasUnbalancedQuotes()) {
        ok = false;
      }
      for (int paramNum = 0; paramNum < tag.getParametersCount(); paramNum++) {
        Parameter tmpParam = tag.getParameter(paramNum);
        if (tmpParam.getValue() == null) {
          ok = false;
        }
      }
      if (ok) {
        return false;
      }
    }

    // Ignore comments
    if (analysis.comments().isAt(currentIndex)) {
      return false;
    }

    // Ignore some tags
    if ((analysis.getSurroundingTag(HtmlTagType.CODE, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.MATH, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.MATH_CHEM, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.NOWIKI, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.PRE, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.SCORE, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.SOURCE, currentIndex) != null) ||
        (analysis.getSurroundingTag(WikiTagType.SYNTAXHIGHLIGHT, currentIndex) != null)) {
      return false;
    }

    // Ensure that it's in the form "<ref name"
    String contents = analysis.getContents();
    int maxLen = contents.length();
    int endIndex = currentIndex + 1;
    while ((endIndex < maxLen) &&
        Character.isWhitespace(contents.charAt(endIndex))) {
      endIndex++;
    }
    if ((endIndex >= maxLen) || !contents.startsWith("ref", endIndex)) {
      return false;
    }
    endIndex += 3;
    if ((endIndex >= maxLen) || !Character.isWhitespace(contents.charAt(endIndex))) {
      return false;
    }
    while ((endIndex < maxLen) && Character.isWhitespace(contents.charAt(endIndex))) {
      endIndex++;
    }
    if ((endIndex >= maxLen) || !contents.startsWith("name", endIndex)) {
      return false;
    }
    endIndex += 4;

    // Report error
    if (errors == null) {
      return true;
    }

    // Compute possible end
    int fullEnd = endIndex;
    if ((fullEnd - 1 < contents.length()) && ("\n<>".indexOf(contents.charAt(fullEnd - 1)) < 0)) {
      while ((fullEnd < contents.length()) && ("\n<>".indexOf(contents.charAt(fullEnd)) < 0)) {
        fullEnd++;
      }
    }
    if (fullEnd >= contents.length()) {
      fullEnd = endIndex;
    } else if (contents.charAt(fullEnd) == '>') {
      fullEnd++;
    } else if (contents.charAt(fullEnd) != '<') {
      fullEnd = endIndex;
    }

    // Report error
    CheckErrorResult errorResult = createCheckErrorResult(
        analysis, currentIndex, fullEnd);
    List<String> replacements = new ArrayList<>();

    // Check if there's an equal sign after "name"
    int equalSign = endIndex;
    while ((equalSign < fullEnd) && (contents.charAt(equalSign) == ' ')) {
      equalSign++;
    }
    if (contents.charAt(equalSign) != '=') {
      equalSign = -1;
    }

    // Case like <ref name=a name>, <ref name=>, <ref name="a name>, <ref name=a name">...
    if (contents.charAt(fullEnd - 1) == '>') {
      int tmpIndex = (equalSign > 0) ? equalSign + 1 : endIndex;
      while ((tmpIndex < fullEnd - 1) && (contents.charAt(tmpIndex) == '=')) {
        tmpIndex++;
      }
      while ((tmpIndex < fullEnd - 1) &&
          (" \"'”“".indexOf(contents.charAt(tmpIndex)) >= 0)) {
        tmpIndex++;
      }
      int startName = tmpIndex;
      boolean finished = false;
      while ((tmpIndex < fullEnd - 1) && !finished) {
        char currentChar = contents.charAt(tmpIndex);
        if (!Character.isLetter(currentChar) &&
            !Character.isDigit(currentChar) &&
            (" -_,.".indexOf(currentChar) < 0)) {
          finished = true;
        }
        if (!finished) {
          tmpIndex++;
        }
      }
      int endName = tmpIndex;
      while ((endName > startName) && (contents.charAt(endName - 1) == ' ')) {
        endName--;
      }
      while ((tmpIndex < fullEnd - 1) &&
          (" \"'”»“".indexOf(contents.charAt(tmpIndex)) >= 0)) {
        tmpIndex++;
      }
      boolean closing = false;
      while ((tmpIndex < fullEnd - 1) && (contents.charAt(tmpIndex) == '/')) {
        tmpIndex++;
        closing = true;
      }
      while ((tmpIndex < fullEnd - 1) &&
          (" \"".indexOf(contents.charAt(tmpIndex)) >= 0)) {
        tmpIndex++;
      }
      if (tmpIndex == fullEnd - 1) {
        String replacement = null;
        boolean automatic = false;
        if (endName > startName) {
          String name = contents.substring(startName, endName);
          if (equalSign > 0) {
            int nextChar = contents.charAt(equalSign + 1);
            if (nextChar == '"') {
              automatic = true;
              for (int charIndex = 0; charIndex < name.length(); charIndex++) {
                if (!Character.isLetterOrDigit(name.charAt(charIndex))) {
                  automatic = false;
                }
              }
            } else if (nextChar == '”') {
              if ("”“".indexOf(contents.charAt(endName)) >= 0) {
                automatic = true;
              }
            } else if (startName == equalSign + 1) {
              if (contents.charAt(endName) == '"') {
                automatic = true;
              }
            }
          }
          replacement =
              contents.substring(currentIndex, endIndex) +
              "=\"" + name + "\"" +
              (closing ? " /" : "") + ">";
        } else {
          replacement = WikiTagType.REF.getOpenTag();
          String original = contents.substring(currentIndex, fullEnd);
          if ("<ref name>".equals(original) ||
              "<ref name=>".equals(original) ||
              "<ref name >".equals(original)) {
            automatic = true;
          }
        }
        if (!replacements.contains(replacement)) {
          replacements.add(replacement);
          errorResult.addReplacement(replacement, automatic);
        }
      } else if (equalSign > 0) {
        tmpIndex = fullEnd - 1;
        while ((tmpIndex > equalSign) &&
               (" ".indexOf(contents.charAt(tmpIndex - 1)) >= 0)) {
          tmpIndex--;
        }
        closing = false;
        if ((tmpIndex > equalSign) && (contents.charAt(tmpIndex - 1) == '/')) {
          closing = true;
          tmpIndex--;
        }
        while ((tmpIndex > equalSign) &&
               (" \u00A0/\"″“”„’»".indexOf(contents.charAt(tmpIndex - 1)) >= 0)) {
          tmpIndex--;
        }
        int endValue = tmpIndex;
        while ((tmpIndex > equalSign) &&
               ("=\"\n".indexOf(contents.charAt(tmpIndex - 1)) < 0)) {
          tmpIndex--;
        }
        if (tmpIndex == equalSign + 1) {
          while ((equalSign < endValue) &&
                 ("= \u00A0\"″“”„‘’»".indexOf(contents.charAt(equalSign)) >= 0)) {
            equalSign++;
          }
          StringBuilder replacement = new StringBuilder();
          replacement.append(contents.substring(currentIndex, endIndex));
          replacement.append("=\"");
          replacement.append(contents.substring(equalSign, endValue));
          replacement.append("\"");
          if (closing) {
            replacement.append(" /");
          }
          replacement.append(">");
          errorResult.addReplacement(replacement.toString(), false);
        }
      }
    }
    errors.add(errorResult);
    return true;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fixUsingAutomaticReplacement(analysis);
  }
}
