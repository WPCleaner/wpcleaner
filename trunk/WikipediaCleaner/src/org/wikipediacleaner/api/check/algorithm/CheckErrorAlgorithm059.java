/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementComment;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
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
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Check that there are <br> tags in the text
    List<PageElementTag> brTags = analysis.getTags(PageElementTag.TAG_HTML_BR);
    if ((brTags == null) || (brTags.isEmpty())) {
      return false;
    }

    // List of templates to be ignored
    List<String> ignoredTemplates = null;
    String tmp = getSpecificProperty("templates", true, true, false);
    if (tmp != null) {
      ignoredTemplates = WPCConfiguration.convertPropertyToStringList(tmp);
    }

    // Analyzing each template
    boolean result = false;
    for (PageElementTemplate template : analysis.getTemplates()) {

      // Check if template should be analyzed
      boolean analyzeTemplate = true;
      if (ignoredTemplates != null) {
        for (String ignoredTemplate : ignoredTemplates) {
          if (Page.areSameTitle(ignoredTemplate, template.getTemplateName())) {
            analyzeTemplate = false;
          }
        }
      }

      // Analyze template
      if (analyzeTemplate) {
        for (int paramNum = 0; paramNum < template.getParameterCount(); paramNum++) {
  
          // Search for <br> at the end of the parameter
          Parameter param = template.getParameter(paramNum);
          String paramValue = param.getValue();
          int paramValueStartIndex = param.getValueStartIndex();
          boolean breakFound = false;
          boolean tagAfter = false;
          int currentValuePos = paramValue.length() - 1;
          int beginError = -1;
          int endError = -1;
          boolean shouldStop = false;
          String replacement = "";
          while (!shouldStop) {
            shouldStop = true;
            currentValuePos = getLastIndexBeforeWhiteSpace(paramValue, currentValuePos);
            if ((currentValuePos > 0) &&
                (paramValue.charAt(currentValuePos) == '>')) {
              PageElementTag tag = analysis.isInTag(
                  paramValueStartIndex +
                  currentValuePos);
              if (tag != null) {
                String name = tag.getNormalizedName();
                if (PageElementTag.TAG_HTML_BR.equals(name)) {
                  breakFound = true;
                  shouldStop = false;
                  beginError = tag.getBeginIndex();
                  if (endError < 0) {
                    endError = tag.getEndIndex();
                  }
                  currentValuePos -= tag.getEndIndex() - tag.getBeginIndex();
                } else if (!breakFound) {
                  if (/*PageElementTag.TAG_WIKI_MATH.equals(name) ||*/
                      PageElementTag.TAG_WIKI_HIERO.equals(name)) {
                    tagAfter = true;
                    shouldStop = false;
                    endError = tag.getCompleteBeginIndex();
                    currentValuePos -= tag.getEndIndex() - tag.getCompleteBeginIndex();
                  }
                }
              } else {
                PageElementComment comment = analysis.isInComment(paramValueStartIndex + currentValuePos);
                if (comment != null) {
                  replacement += analysis.getContents().substring(comment.getBeginIndex(), comment.getEndIndex());
                  shouldStop = false;
                  currentValuePos -= comment.getEndIndex() - comment.getBeginIndex();
                }
              }
            }
          }
  
          // Report error
          if (breakFound) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, beginError, endError,
                (tagAfter ? ErrorLevel.WARNING : ErrorLevel.ERROR));
            if (!tagAfter) {
              errorResult.addReplacement(replacement);
            }
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }

  /**
   * Return the parameters used to configure the algorithm.
   * 
   * @return Map of parameters (Name -> description).
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put(
        "templates",
        GT._("A list of templates that should be ignored"));
    return parameters;
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
