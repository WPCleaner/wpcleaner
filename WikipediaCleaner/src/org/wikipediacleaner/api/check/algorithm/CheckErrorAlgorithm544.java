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
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.contents.IntervalComparator;


/**
 * Algorithm for analyzing error 544 of check wikipedia project.
 * Error 544: Missing end model of a pair.
 */
public class CheckErrorAlgorithm544 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm544() {
    super("Missing end model of a pair");
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

    // Retrieve configuration
    String tmp = getSpecificProperty("pair_templates", true, true, false);
    if ((tmp == null) || tmp.isEmpty()) {
      return false;
    }
    List<String[]> pairs = WPCConfiguration.convertPropertyToStringArrayList(tmp);
    if ((pairs == null) || (pairs.isEmpty())) {
      return false;
    }
    
    // Analyze each pair
    boolean result = false;
    for (String[] pair : pairs) {
      if (pair != null) {
  
        // Retrieve open templates
        List<PageElementTemplate> openTemplates = null;
        if (pair.length > 0) {
          openTemplates = analysis.getTemplates(pair[0]);
        }
  
        // Retrieve close templates
        List<PageElementTemplate> closeTemplates = null;
        if ((openTemplates != null) &&
            !openTemplates.isEmpty()) {
          closeTemplates = new ArrayList<>();
          for (int i = 1; i < pair.length; i++) {
            String closeTemplate = pair[i];
            List<PageElementTemplate> tmpTemplates = analysis.getTemplates(closeTemplate);
            if (tmpTemplates != null) {
              closeTemplates.addAll(tmpTemplates);
            }
          }
          closeTemplates.sort(new IntervalComparator());
        }
  
        // Match templates together
        if ((openTemplates != null) &&
            !openTemplates.isEmpty() &&
            (closeTemplates != null)) {
          for (int i = openTemplates.size(); i > 0; i--) {
            PageElementTemplate openTemplate = openTemplates.get(i - 1);
            PageElementTemplate closeTemplate = closeTemplates.isEmpty() ? null : closeTemplates.get(closeTemplates.size() - 1);
            if ((closeTemplate != null) &&
                (closeTemplate.getBeginIndex() >= openTemplate.getEndIndex())) {
              closeTemplates.remove(closeTemplates.size() - 1);
              openTemplates.remove(i - 1);
            }
          }
        }

        // Report errors
        if ((openTemplates != null) &&
            !openTemplates.isEmpty()) {
          result = true;
          if (errors == null) {
            return true;
          }

          for (PageElementTemplate openTemplate : openTemplates) {
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, openTemplate.getBeginIndex(), openTemplate.getEndIndex());
            errors.add(errorResult);
          }
        }
      }
    }

    return result;
  }
}
