/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 16 of check wikipedia project.
 * Error 16: Template with Unicode control characters
 */
public class CheckErrorAlgorithm016 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Remove all control characters"),
  };

  private final static char ZERO_WIDTH_BREAK = 0x200B;
  private final static char LEFT_TO_RIGHT_MARK = 0x200E;
  private final static char ZERO_WIDTH_NO_BREAK = 0xFEFF;

  /**
   * All control character.
   */
  private final static String controlCharacters =
      "" +
      ZERO_WIDTH_NO_BREAK +
      LEFT_TO_RIGHT_MARK +
      ZERO_WIDTH_BREAK;

  public CheckErrorAlgorithm016() {
    super("Template with Unicode control characters");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }

    // Retrieve configuration
    boolean onlyTemplates = Boolean.valueOf(getSpecificProperty("only_templates", true, true, false));

    boolean result = false;
    String contents = pageAnalysis.getContents();

    if (onlyTemplates) {
      Collection<PageElementTemplate> templates = pageAnalysis.getTemplates();
      if (templates == null) {
        return false;
      }
      int lastEnd = 0;
      for (PageElementTemplate template : templates) {
        int begin = template.getBeginIndex();
        int end = template.getEndIndex();
        if (begin >= lastEnd) {
          boolean found = false;
          for (int index = begin; index < end; index++) {
            char character = contents.charAt(index);
            if (controlCharacters.indexOf(character) >= 0) {
              found = true;
            }
          }
          if (found) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(pageAnalysis.getPage(), begin, end);
            StringBuilder replacement = new StringBuilder();
            for (int index = begin; index < end; index++) {
              char character = contents.charAt(index);
              if (controlCharacters.indexOf(character) < 0) {
                replacement.append(character);
              }
            }
            errorResult.addReplacement(
                replacement.toString(),
                GT._("Remove all control characters"),
                canBeAutomatic(replacement.toString()));
            errors.add(errorResult);
          }
          lastEnd = end;
        }
      }
    } else {
      int index = 0;
      while (index < contents.length()) {
        int character = contents.codePointAt(index);
        if (controlCharacters.indexOf(character) >= 0) {
          if (errors == null) {
            return true;
          }
          result = true;
          int begin = Math.max(index - Character.charCount(contents.codePointBefore(index)), 0);
          boolean finished = false;
          while ((begin >= 0) && !finished) {
            int before = contents.codePointBefore(begin);
            if (controlCharacters.indexOf(before) >= 0) {
              begin -= Character.charCount(before);
            } else {
              finished = true;
            }
          }
          int end = Math.min(index + 1, contents.length() - 1);
          finished = false;
          while ((end + 1 < contents.length()) && !finished) {
            int after = contents.codePointAt(end);
            if (controlCharacters.indexOf(after) >= 0) {
              end += Character.charCount(after);
            } else {
              finished = true;
            }
          }
          if (end < contents.length()) {
            end += Character.charCount(contents.codePointAt(end));
          }
          CheckErrorResult errorResult = createCheckErrorResult(pageAnalysis.getPage(), begin, end);
          StringBuilder replacement = new StringBuilder();
          int i = begin;
          while (i < end) {
            character = contents.codePointAt(i);
            if (controlCharacters.indexOf(character) < 0) {
              replacement.appendCodePoint(character);
            }
            i += Character.charCount(character);
          }
          errorResult.addReplacement(
              replacement.toString(),
              GT._("Remove all control characters"),
              canBeAutomatic(replacement.toString()));
          errors.add(errorResult);
          index = end + 2;
        } else {
          index += Character.charCount(character);
        }
      }
    }

    return result;
  }

  /**
   * Authorized characters for automatic replacement.
   */
  private final static String automatic =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
      "abcdefghijklmnopqrstuvwxyz" +
      "áàéèêïōù" +
      "0123456789" +
      " []|()<>,.!?;:-–=+/'\n";

  /**
   * @param replacement Replacement.
   * @return True if replacement can be applied automatically.
   */
  private boolean canBeAutomatic(String replacement) {
    if (replacement == null) {
      return false;
    }
    int i = 0;
    while (i < replacement.length()) {
      int codePoint = replacement.codePointAt(i);
      if (automatic.indexOf(codePoint) < 0) {
        return false;
      }
      i += Character.charCount(codePoint);
    }
    return true;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  public String automaticFix(PageAnalysis analysis) {
    return fix(globalFixes[0], analysis, null);
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {
    return fixUsingAutomaticReplacement(analysis);
  }

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("only_templates", GT._("To report control characters only in templates"));
    return parameters;
  }
}
