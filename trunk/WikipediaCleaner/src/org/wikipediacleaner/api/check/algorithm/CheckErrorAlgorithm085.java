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
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 85 of check wikipedia project.
 * Error 85: Tag without content
 */
public class CheckErrorAlgorithm085 extends CheckErrorAlgorithmBase {

  private final static String[] interestingTags = {
    PageElementTag.TAG_HTML_CENTER,
    PageElementTag.TAG_WIKI_INCLUDEONLY,
    PageElementTag.TAG_WIKI_GALLERY,
    PageElementTag.TAG_WIKI_NOINCLUDE,
    PageElementTag.TAG_WIKI_REF,
  };

  private final static String[] ignoredTags = {
    PageElementTag.TAG_WIKI_CODE,
    PageElementTag.TAG_WIKI_NOWIKI,
    PageElementTag.TAG_WIKI_PRE,
    PageElementTag.TAG_WIKI_SCORE,
  };

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Delete all tags without content"),
  };

  public CheckErrorAlgorithm085() {
    super("Tag without content");
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

    // Check each tag
    List<PageElementTag> tags = analysis.getTags();
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementTag tag : tags) {
      if (!tag.isFullTag() && !tag.isEndTag() && tag.isComplete()) {

        // Check if tag can be of interest
        boolean interesting = false;
        for (String tagName : interestingTags) {
          if (tagName.equals(tag.getName())) {
            interesting = true;
          }
        }
  
        // Check tag
        if (interesting) {
          ErrorLevel errorLevel = ErrorLevel.ERROR;

          // Check if text is found inside the tag
          boolean textFound = false;
          boolean ignoredText = false;
          int currentIndex = tag.getValueBeginIndex();
          int lastIndex = tag.getValueEndIndex();
          while (!textFound && (currentIndex < lastIndex)) {
            char currentChar = contents.charAt(currentIndex);
            if (Character.isWhitespace(currentChar)) {
              currentIndex++;
            } else if (currentChar == '<') {
              boolean ok = false;
              if (!ok) {
                PageElementTag internalTag = analysis.isInTag(currentIndex);
                if (internalTag != null) {
                  for (String ignoredTag : ignoredTags) {
                    if (ignoredTag.equals(internalTag.getName())) {
                      ok = true;
                      ignoredText = true;
                      errorLevel = ErrorLevel.WARNING;
                      currentIndex = internalTag.getCompleteEndIndex();
                    }
                  }
                }
              }
              if (!ok) {
                textFound = true;
              }
            } else {
              textFound = true;
            }
          }

          // Check if tag has arguments
          boolean hasArguments = false;
          if (!textFound) {
            if (tag.getParametersCount() > 0) {
              if (PageElementTag.TAG_WIKI_REF.equals(tag.getName())) {
                hasArguments = true;
              }
            }
          }

          if (!textFound && !hasArguments) {
            if (errors == null) {
              return true;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis,
                tag.getCompleteBeginIndex(),
                tag.getCompleteEndIndex(),
                errorLevel);
            if (!ignoredText) {
              errorResult.addReplacement("");
            } else {
              if (PageElementTag.TAG_HTML_CENTER.equals(tag.getName())) {
                String templatesProp = getSpecificProperty("center_templates", true, true, false);
                if (templatesProp != null) {
                  List<String[]> templates = WPCConfiguration.convertPropertyToStringArrayList(templatesProp);
                  if (templates != null) {
                    for (String[] template : templates) {
                      if (template.length > 1) {
                        StringBuilder replacement = new StringBuilder();
                        replacement.append("{{");
                        replacement.append(template[0]);
                        replacement.append("|");
                        replacement.append(template[1]);
                        replacement.append("=");
                        replacement.append(contents.substring(
                            tag.getValueBeginIndex(), tag.getValueEndIndex()));
                        replacement.append("}}");
                        errorResult.addReplacement(
                            replacement.toString(),
                            GT._("Use {0}", PageElementTemplate.createTemplate(template[0])));
                      }
                    }
                  }
                }
              }
            }
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

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("center_templates", GT._("A list of templates that can be used to replace &lt;center&gt; tags"));
    return parameters;
  }
}
