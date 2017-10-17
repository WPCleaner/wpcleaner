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
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementImage.Parameter;


/**
 * Algorithm for analyzing error 534 of check wikipedia project.
 * Error 534: Bogus image options (see [[Special:LintErrors/bogus-image-options]])
 */
public class CheckErrorAlgorithm534 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm534() {
    super("Bogus image options");
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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Analyze each image
    List<PageElementImage> images = analysis.getImages();
    String contents = analysis.getContents();
    boolean result = false;
    ArrayList<Parameter> parameters = new ArrayList<>();
    for (PageElementImage image : images) {

      // Check how many parameters can't be related to a magic word
      parameters.clear();
      Collection<Parameter> imageParameters = image.getParameters();
      if (imageParameters != null) {
        for (Parameter param : image.getParameters()) {
          if ((param != null) && (param.getMagicWord() == null)) {
            parameters.add(param);
          }
        }
      }

      // Report images whit several parameters that can't be related to a magic word
      if (parameters.size() > 1) {
        result = true;
        if (errors == null) {
          return result;
        }
        for (int numParam = 0; numParam < parameters.size(); numParam++) {
          Parameter param = parameters.get(numParam);
          int beginIndex = image.getBeginIndex() + param.getBeginOffset();
          int endIndex = image.getBeginIndex() + param.getEndOffset();
          if (numParam == parameters.size() - 1) {
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, beginIndex, endIndex, ErrorLevel.CORRECT);
            errors.add(errorResult);
          } else {
            boolean hasContents = false;
            for (int index = beginIndex; index < endIndex; index++) {
              if (contents.charAt(index) != ' ') {
                hasContents = true;
              }
            }
            if (!hasContents) {
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, beginIndex - 1, endIndex);
              errorResult.addReplacement("", true);
              errors.add(errorResult);
            } else {
              CheckErrorResult errorResult = createCheckErrorResult(
                  analysis, beginIndex, endIndex);
              errors.add(errorResult);
            }
          }
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
    return fixUsingAutomaticReplacement(analysis);
  }
}
