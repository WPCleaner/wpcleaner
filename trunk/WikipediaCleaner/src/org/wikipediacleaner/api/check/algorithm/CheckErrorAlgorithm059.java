/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 59 of check wikipedia project.
 * Error 59: Template value end with break
 */
public class CheckErrorAlgorithm059 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Delete all"),
  };

  public CheckErrorAlgorithm059() {
    super("Template value end with break");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (pageAnalysis == null) {
      return false;
    }

    // Retrieve list of <br> tags
    List<PageElementTag> brTags = pageAnalysis.getTags(PageElementTag.TAG_HTML_BR);
    int brTagsSize = (brTags != null) ? brTags.size() : 0;
    int currentBrTag = 0;
    if ((brTags == null) || (brTagsSize == 0)) {
      return false;
    }

    // Analyzing from the beginning
    boolean errorFound = false;
    String contents = pageAnalysis.getContents();
    for (PageElementTemplate template : pageAnalysis.getTemplates()) {

      // Find the first <br> tag after template begin
      while ((currentBrTag < brTagsSize) &&
             (brTags.get(currentBrTag).getBeginIndex() < template.getBeginIndex())) {
        currentBrTag++;
      }
      if (currentBrTag >= brTagsSize) {
        return errorFound;
      }
 
      // Check if template has <br> tags in it
      if (brTags.get(currentBrTag).getBeginIndex() < template.getEndIndex()) {

        // Check every parameter
        for (int i = 0; i < template.getParameterCount(); i++) {

          String parameterValue = template.getParameterValue(i);
          if (parameterValue != null) {

            // Find the last <br> tag in parameter
            int lastParamIndex = template.getParameterValueOffset(i) + parameterValue.length();
            PageElementTag lastBrTag = null;
            while ((currentBrTag < brTagsSize) &&
                   (brTags.get(currentBrTag).getBeginIndex() < lastParamIndex)) {
              lastBrTag = brTags.get(currentBrTag);
              currentBrTag++;
            }

            // Check if a <br> tag is at the end of the parameter value
            if (lastBrTag != null) {
              int currentIndex = lastBrTag.getEndIndex();
              boolean ok = true;
              while (currentIndex < lastParamIndex) {
                if (Character.isWhitespace(contents.charAt(currentIndex))) {
                  currentIndex++;
                } else {
                  PageElementComment comment = pageAnalysis.isInComment(currentIndex);
                  if (comment != null) {
                    currentIndex = comment.getEndIndex();
                  } else {
                    currentIndex = lastParamIndex;
                    ok = false;
                  }
                }
              }
              if (ok) {
                if (errors == null) {
                  return true;
                }
                errorFound = true;
                int firstBrTagIndex = currentBrTag - 1;
                while ((firstBrTagIndex > 0) &&
                       (PageElementTag.groupTags(brTags, firstBrTagIndex - 1, contents, null, null) >= currentBrTag - 1)) {
                  firstBrTagIndex--;
                }
                PageElementTag firstBrTag = brTags.get(firstBrTagIndex);
                CheckErrorResult errorResult = createCheckErrorResult(
                    pageAnalysis.getPage(),
                    firstBrTag.getBeginIndex(),
                    lastBrTag.getEndIndex());
                errorResult.addReplacement("", GT._("Delete"));
                errors.add(errorResult);
              }
            }
          }
        }
      }
    }

    // Result
    return errorFound;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  // No Bot fixing
  // @Override
  //public String botFix(PageAnalysis analysis) {
  //  return fix(globalFixes[0], analysis, null);
  //}

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
