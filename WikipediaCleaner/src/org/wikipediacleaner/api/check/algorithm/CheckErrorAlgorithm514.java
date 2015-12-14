/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 511 of check wikipedia project.
 * Error 514: Missing named reference
 */
public class CheckErrorAlgorithm514 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm514() {
    super("Missing named reference");
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
    if ((analysis == null) || (analysis.getInternalLinks() == null)) {
      return false;
    }

    // List named references with actual content 
    boolean result = false;
    List<PageElementTag> tags = analysis.getCompleteTags(PageElementTag.TAG_WIKI_REF);
    if (tags == null) {
      return result;
    }
    Map<String, Set<String>> names = new HashMap<String, Set<String>>();
    for (PageElementTag tag : tags) {
      if (!tag.isFullTag() && !tag.isEndTag() && tag.isComplete()) {
        String value = analysis.getContents().substring(tag.getValueBeginIndex(), tag.getValueEndIndex());
        if ((value != null) && (value.trim().length() > 0)) {
          PageElementTag.Parameter nameParam = tag.getParameter("name");
          if (nameParam != null) {
            String groupValue = tag.getGroupOfRef(analysis);
            Set<String> set = names.get(groupValue);
            if (set == null) {
              set = new HashSet<String>();
              names.put(groupValue, set);
            }
            String nameValue = nameParam.getTrimmedValue();
            set.add(nameValue);
          }
        }
      }
    }

    // Check named references with missing content
    for (PageElementTag tag : tags) {
      boolean withoutContent = false;
      if (tag.isFullTag()) {
        withoutContent = true;
      } else if (tag.isComplete()) {
        String content = analysis.getContents().substring(tag.getValueBeginIndex(), tag.getValueEndIndex());
        withoutContent = ((content == null) || (content.trim().length() == 0));
      }
      if (withoutContent) {
        PageElementTag.Parameter nameParam = tag.getParameter("name");
        if (nameParam != null) {
          String nameValue = nameParam.getTrimmedValue();
          PageElementTag.Parameter groupParam = tag.getParameter("group");
          String groupValue = (groupParam != null) ? groupParam.getValue() : null;
          if ((groupValue != null) && (groupValue.length() == 0)) {
            groupValue = null;
          }
          boolean found = false;
          Set<String> set = names.get(groupValue);
          if (set != null) {
            found = set.contains(nameValue);
          }
          if (!found && (groupValue == null)) {
            String refByTemplate = getSpecificProperty("ref_by_template", true, true, false);
            if (refByTemplate != null) {
              List<String> refByTemplateList = WPCConfiguration.convertPropertyToStringList(refByTemplate);
              if (refByTemplateList != null) {
                for (String refByTemplateElement : refByTemplateList) {
                  String[] elements = refByTemplateElement.split("\\|");
                  for (int numElement = 1; numElement < elements.length; numElement++) {
                    if (elements[numElement].equals(nameValue)) {
                      List<PageElementTemplate> templates = analysis.getTemplates(elements[0]);
                      if ((templates != null) && !templates.isEmpty()) {
                        found = true;
                      }
                    }
                  }
                }
              }
            }
          }
          if (!found) {
            if (errors == null) {
              return false;
            }
            result = true;
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
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
        "ref_by_template",
        GT._("References defined by templates."));
    return parameters;
  }
}
