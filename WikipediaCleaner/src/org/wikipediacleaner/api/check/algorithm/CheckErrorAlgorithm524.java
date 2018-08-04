/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.constants.WPCConfiguration;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 524 of check wikipedia project.
 * Error 524: Duplicate template argument
 */
public class CheckErrorAlgorithm524 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm524() {
    super("Duplicate template argument");
  }

  /**
   * Tracking category.
   */
  private String trackingCategory;

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

    // Retrieve configuration
    String ignoreString = getSpecificProperty("ignore", true, true, false);
    List<String[]> ignore = WPCConfiguration.convertPropertyToStringArrayList(ignoreString);

    // Analyze each template
    List<PageElementTemplate> templates = analysis.getTemplates();
    if ((templates == null) || templates.isEmpty()) {
      return false;
    }
    HashMap<String, ParameterInfo> names = new HashMap<String, ParameterInfo>();
    List<ParameterInfo> duplicates = new ArrayList<ParameterInfo>();
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementTemplate template : templates) {
      boolean shouldCheck = true;
      int nbParam = template.getParameterCount();
      if (shouldCheck && (nbParam < 1)) {
        shouldCheck = false;
      }
      if (shouldCheck) {
        if (analysis.isInTag(template.getBeginIndex(), PageElementTag.TAG_WIKI_PRE) != null) {
          shouldCheck = false;
        }
      }
      if (shouldCheck) {
        names.clear();
        duplicates.clear();
        for (int numParam = 0; numParam < nbParam; numParam++) {
          Parameter param = template.getParameter(numParam);
          String paramName = param.getComputedName();
          ParameterInfo existingParam = names.get(paramName);
          ParameterInfo newParam = new ParameterInfo(numParam, param);
          names.put(paramName, newParam);
          if (existingParam != null) {
            if (errors == null) {
              return true;
            }
            result = true;

            duplicates.remove(existingParam);
            duplicates.add(newParam);

            // Compute actual area
            int pipeBefore = existingParam.param.getPipeIndex();
            int paramBegin = pipeBefore;
            Parameter nextParam = template.getParameter(existingParam.numParam + 1);
            int paramEnd = nextParam.getPipeIndex();
            boolean existingStartNewLine = false;
            int tmpIndex = getLastIndexBeforeSpace(contents, paramBegin - 1);
            if ((tmpIndex >= 0) && (contents.charAt(tmpIndex) == '\n')) {
              existingStartNewLine = true;
            }
            boolean existingEndNewLine = false;
            tmpIndex = getLastIndexBeforeSpace(contents, paramEnd - 1);
            if ((tmpIndex >= 0) && (contents.charAt(tmpIndex) == '\n')) {
              existingEndNewLine = true;
            }
            if (!existingStartNewLine && existingEndNewLine) {
              paramEnd = tmpIndex;
            }

            // Create error
            CheckErrorResult errorResult = createCheckErrorResult(
                analysis, paramBegin, paramEnd);
            String existingValue = existingParam.param.getValue();
            String value = param.getValue();

            boolean automatic = true;
            if (automatic) {
              if (paramName.trim().length() == 0) {
                automatic = false;
              }
            }
            if (automatic) {
              // Detect special cases: first parameter unnamed, second one explicitly named
              boolean special = false;
              String existingName = existingParam.param.getName();
              if (((existingName == null) || (existingName.trim().length() == 0)) &&
                  (param.getName() != null) &&
                  (param.getName().trim().length() > 0)) {
                special = true;
                // Avoid unnamed parameter in between
                for (int numParam2 = existingParam.numParam + 1; numParam2 < numParam; numParam2++) {
                  String name2 = template.getParameter(numParam2).getName();
                  if ((name2 == null) || (name2.trim().length() == 0)) {
                    special = false;
                  }
                }
              }

              // If the argument name contains digits, don't do automatic replacement
              if (!special) {
                for (int pos = 0; pos < paramName.length(); pos++) {
                  char currentChar = paramName.charAt(pos);
                  if (Character.isDigit(currentChar)) {
                    automatic = false;
                  }
                }
              }
            }

            boolean ignored = false;
            if ((ignore != null) && !ignore.isEmpty()) {
              // Manage some parameters safe to replace
              for (String[] ignoreElement : ignore) {
                if ((ignoreElement.length > 1) &&
                    Page.areSameTitle(template.getTemplateName(), ignoreElement[0]) &&
                    ignoreElement[1].equals(paramName)) {
                  if (ignoreElement.length > 2) {
                    for (int pos = 2; pos < ignoreElement.length; pos++) {
                      if (ignoreElement[pos].equals(existingValue)) {
                        ignored = true;
                      }
                    }
                  }
                }
              }
            }

            if (automatic) {
              // If there's a table start, don't do automatic replacement
              int indexTable = contents.indexOf("{|", template.getBeginIndex() + 2);
              if ((indexTable >= 0) && (indexTable < paramBegin)) {
                automatic = false;
              }
            }
            if (automatic) {
              // If there's a table new line, don't do automatic replacement
              int indexTable = contents.indexOf("|-", template.getBeginIndex() + 2);
              if ((indexTable >= 0) && (indexTable < paramBegin)) {
                automatic = false;
              }
            }

            // Suggestions to remove the parameter
            if ((existingValue != null) && (existingValue.equals(value))) {
              errorResult.addReplacement("", automatic);
            } else if (("".equals(existingValue))) {
              errorResult.addReplacement("", automatic);
            } else if (ignored) {
              errorResult.addReplacement("", automatic);
            }

            // Suggestions to comment the parameter
            if ((existingValue != null) && (!"".equals(existingValue))) {
              boolean numericName = true;
              for (int i = 0; i < paramName.length(); i++) {
                if (!Character.isDigit(paramName.charAt(i))) {
                  numericName = false;
                }
              }
              if (!numericName) {
                StringBuilder replacement = new StringBuilder();
                if (paramBegin < pipeBefore) {
                  replacement.append(contents.substring(paramBegin, pipeBefore));
                }
                replacement.append("<!--");
                tmpIndex = paramEnd;
                while ((tmpIndex > pipeBefore) &&
                       Character.isWhitespace(contents.charAt(tmpIndex - 1))) {
                  tmpIndex--;
                }
                replacement.append(contents.substring(pipeBefore, tmpIndex));
                replacement.append("-->");
                if (paramEnd > tmpIndex) {
                  replacement.append(contents.substring(tmpIndex, paramEnd));
                }
                errorResult.addReplacement(replacement.toString(), GT._T("Comment"));
              }
            }

            errors.add(errorResult);
          }
        }

        // Mark duplicates
        for (ParameterInfo paramInfo : duplicates) {
          int endIndex = template.getEndIndex() - 2;
          if (paramInfo.numParam + 1 < template.getParameterCount()) {
            endIndex = template.getParameter(paramInfo.numParam + 1).getPipeIndex();
          }
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis,
              paramInfo.param.getPipeIndex(), endIndex,
              ErrorLevel.CORRECT);
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * @return True if the error has a special list of pages.
   */
  @Override
  public boolean hasSpecialList() {
    return (getTrackingCategory() != null);
  }

  /**
   * @param category Tracking category.
   */
  public void setTrackingCategory(String category) {
    trackingCategory = category;
  }

  /**
   * @return Tracking category.
   */
  private String getTrackingCategory() {
    String categoryName = getSpecificProperty("category", true, true, false);
    if ((categoryName != null) &&
        (categoryName.trim().length() > 0)) {
      return categoryName;
    }
    if ((trackingCategory != null) &&
        (trackingCategory.trim().length() > 0)) {
      return trackingCategory;
    }
    return null;
  }

  /**
   * Retrieve the list of pages in error.
   * 
   * @param wiki Wiki.
   * @param limit Maximum number of pages to retrieve.
   * @return List of pages in error.
   */
  @Override
  public List<Page> getSpecialList(EnumWikipedia wiki, int limit) {
    List<Page> result = null;
    String categoryName = getTrackingCategory();
    if (categoryName != null) {
      API api = APIFactory.getAPI();
      String title = wiki.getWikiConfiguration().getPageTitle(Namespace.CATEGORY, categoryName);
      Page category = DataManager.getPage(wiki, title, null, null, null);
      try {
        api.retrieveCategoryMembers(wiki, category, 0, false, limit);
        result = category.getRelatedPages(RelatedPages.CATEGORY_MEMBERS);
      } catch (APIException e) {
        //
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

  /**
   * @return Map of parameters (key=name, value=description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("category", GT._T("A category containing the list of pages in error"));
    parameters.put("ignore", GT._T("Values that can be safely ignored for a given template and argument"));
    return parameters;
  }

  /**
   * Bean for holding information about a parameter
   */
  private static class ParameterInfo {
    public final int numParam;
    public final Parameter param;
    public ParameterInfo(int numParam, Parameter param) {
      this.numParam = numParam;
      this.param = param;
    }
  }
}
