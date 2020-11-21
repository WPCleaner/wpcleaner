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
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.tag.TagBuilder;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 66 of check wikipedia project.
 * Error 66: Image description with full &lt;small&gt;.
 */
public class CheckErrorAlgorithm066 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._T("Remove all <small> tags"),
  };

  public CheckErrorAlgorithm066() {
    super("Image description with full <small>.");
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

    // Analyzing all images
    boolean result = false;
    List<PageElementImage> images = analysis.getImages();
    for (PageElementImage image : images) {
      String description = image.getDescription();
      if (description != null) {
        description = description.trim();
        PageAnalysis descAnalysis = analysis.getPage().getAnalysis(description, false);
        List<PageElementTag> smallTags = descAnalysis.getTags(PageElementTag.TAG_HTML_SMALL);
        if ((smallTags != null) && (!smallTags.isEmpty())) {
          int lastTest = 0;
          int currentDepth = 0;
          boolean onlySmall = true;
          StringBuilder innerText = new StringBuilder();
          for (PageElementTag smallTag : smallTags) {
            if ((currentDepth == 0) && (smallTag.getBeginIndex() > lastTest)) {
              onlySmall = false;
            }
            if (smallTag.getBeginIndex() > lastTest) {
              innerText.append(description.substring(lastTest, smallTag.getBeginIndex()));
            }
            lastTest = smallTag.getEndIndex();
            if (!smallTag.isFullTag()) {
              if (smallTag.isEndTag()) {
                currentDepth = Math.max(0, currentDepth - 1);
              } else {
                currentDepth++;
              }
            }
          }
          if (onlySmall) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, image.getBeginIndex(), image.getEndIndex());
            errorResult.addReplacement(
                image.getDescriptionReplacement(innerText.toString()),
                GT._T(
                    "Remove {0} tags",
                    TagBuilder.SMALL_OPEN));
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }

  /**
   * Bot fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalBotFix(PageAnalysis analysis) {
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
    return fixUsingFirstReplacement(fixName, analysis);
  }
}
