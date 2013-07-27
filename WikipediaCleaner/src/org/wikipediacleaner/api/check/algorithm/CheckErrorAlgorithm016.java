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

    Collection<PageElementTemplate> templates = pageAnalysis.getTemplates();
    if (templates == null) {
      return false;
    }
    boolean result = false;
    String contents = pageAnalysis.getContents();
    int lastEnd = 0;
    for (PageElementTemplate template : templates) {
      int begin = template.getBeginIndex();
      int end = template.getEndIndex();
      if (begin >= lastEnd) {
        boolean found = false;
        for (int index = begin; index < end; index++) {
          char character = contents.charAt(index);
          if ((character == (char) 0xFEFF) ||
              (character == (char) 0x200E) ||
              (character == (char) 0x200B)) {
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
            if ((character != (char) 0xFEFF) &&
                (character != (char) 0x200E) &&
                (character != (char) 0x200B)) {
              replacement.append(character);
            }
          }
          errorResult.addReplacement(replacement.toString(), GT._("Remove all control characters"));
          errors.add(errorResult);
        }
        lastEnd = end;
      }
    }
    return result;
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
    return fixUsingFirstReplacement(fixName, analysis);
  }
}
