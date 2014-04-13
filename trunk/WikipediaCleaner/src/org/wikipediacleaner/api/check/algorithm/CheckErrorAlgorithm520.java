/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;


/**
 * Algorithm for analyzing error 520 of check wikipedia project.
 * Error 520: Weird characters (pawns, snowmen) in main namespace
 */
public class CheckErrorAlgorithm520 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm520() {
    super("Weird characters");
  }

  /**
   * Weird characters to look for.
   */
  private final static String weirdCharacters = "♙☃"; // pawns, snowmen

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }
    Integer ns = analysis.getPage().getNamespace();
    if ((ns == null) || (ns.intValue() != Namespace.MAIN)) {
      return false;
    }

    // Search weird characters
    String contents = analysis.getContents();
    boolean result = false;
    for (int index = 0; index < contents.length(); index++) {
      if (weirdCharacters.indexOf(contents.charAt(index)) >= 0) {
        if (errors == null) {
          return true;
        }
        result = true;
        CheckErrorResult errorResult = createCheckErrorResult(
            analysis, index, index + 1);
        errorResult.addReplacement("");
        errors.add(errorResult);
      }
    }

    return result;
  }
}
