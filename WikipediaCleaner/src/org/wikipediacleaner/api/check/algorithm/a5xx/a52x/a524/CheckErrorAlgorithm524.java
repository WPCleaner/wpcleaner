/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm.a5xx.a52x.a524;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.wikipediacleaner.api.API;
import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.APIFactory;
import org.wikipediacleaner.api.algorithm.AlgorithmParameter;
import org.wikipediacleaner.api.algorithm.AlgorithmParameterElement;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase;
import org.wikipediacleaner.api.check.algorithm.a5xx.TemplateConfigurationGroup;
import org.wikipediacleaner.api.configuration.WPCConfiguration;
import org.wikipediacleaner.api.configuration.WPCConfigurationStringList;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.DataManager;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.api.data.Page.RelatedPages;
import org.wikipediacleaner.api.data.PageElementTemplate.Parameter;
import org.wikipediacleaner.api.data.analysis.PageAnalysis;
import org.wikipediacleaner.api.data.contents.ContentsUtil;
import org.wikipediacleaner.api.data.contents.comment.CommentBuilder;
import org.wikipediacleaner.api.data.contents.tag.WikiTagType;
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

    // Analyze each template
    List<PageElementTemplate> templates = analysis.getTemplates();
    if ((templates == null) || templates.isEmpty()) {
      return false;
    }
    HashMap<String, ParameterInfo> names = new HashMap<>();
    List<ParameterInfo> duplicates = new ArrayList<>();
    boolean result = false;
    String contents = analysis.getContents();
    for (PageElementTemplate template : templates) {
      boolean shouldCheck = true;
      int nbParam = template.getParameterCount();
      if (nbParam < 1) {
        shouldCheck = false;
      }
      if (shouldCheck) {
        if (analysis.isInTag(template.getBeginIndex(), WikiTagType.PRE) != null) {
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
            int tmpIndex = ContentsUtil.moveIndexBeforeWhitespace(contents, paramBegin - 1);
            if ((tmpIndex >= 0) && (contents.charAt(tmpIndex) == '\n')) {
              existingStartNewLine = true;
            }
            boolean existingEndNewLine = false;
            tmpIndex = ContentsUtil.moveIndexBeforeWhitespace(contents, paramEnd - 1);
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
            boolean identical = identicalParameters.stream()
                .filter(identicalParameter -> Page.areSameTitle(identicalParameter.templateName, template.getTemplateName()))
                .filter(identicalParameter -> Objects.equals(identicalParameter.parameterName, paramName))
                .filter(identicalParameter -> identicalParameter.values.contains(existingValue))
                .anyMatch(identicalParameter -> identicalParameter.values.contains(value));

            // Display the value of the existing parameter
            if (value.equals(existingValue)) {
              errorResult.addText(GT._T("Both values are equal"));
            } else if (value.isEmpty()) {
              errorResult.addText(GT._T("Other value is empty"));
            } else if (identical) {
              errorResult.addText(GT._T("Both values are identical"));
            } else {
              errorResult.addText(GT._T("Other value: {0}", value));
            }

            boolean automatic = !paramName.trim().isEmpty();
            if (automatic) {
              // Detect special cases: first parameter unnamed, second one explicitly named
              boolean special = false;
              String existingName = existingParam.param.getName();
              if (((existingName == null) || existingName.trim().isEmpty()) &&
                  (param.getName() != null) &&
                  !param.getName().trim().isEmpty()) {
                special = true;
                // Avoid unnamed parameter in between
                for (int numParam2 = existingParam.numParam + 1; numParam2 < numParam; numParam2++) {
                  String name2 = template.getParameter(numParam2).getName();
                  if ((name2 == null) || name2.trim().isEmpty()) {
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

            // Manage some parameters safe to replace
            boolean ignored = false;
            for (String[] ignoreElement : ignore) {
              if ((ignoreElement.length > 1) &&
                  Page.areSameTitle(template.getTemplateName(), ignoreElement[0]) &&
                  ignoreElement[1].equals(paramName)) {
                if (ignoreElement.length > 2) {
                  for (int pos = 2; pos < ignoreElement.length; pos++) {
                    if (ignoreElement[pos].equals(existingValue)) {
                      ignored = true;
                      break;
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
            } else if (identical) {
              errorResult.addReplacement("", automatic);
            } else if (ignored) {
              errorResult.addReplacement("", automatic);
            }

            // Suggestions to comment the parameter
            if ((existingValue != null) && !existingValue.isEmpty()) {
              boolean numericName = true;
              for (int i = 0; i < paramName.length(); i++) {
                if (!Character.isDigit(paramName.charAt(i))) {
                  numericName = false;
                }
              }
              if (!numericName) {
                StringBuilder replacement = new StringBuilder();
                if (paramBegin < pipeBefore) {
                  replacement.append(contents, paramBegin, pipeBefore);
                }
                tmpIndex = paramEnd;
                while ((tmpIndex > pipeBefore) &&
                       Character.isWhitespace(contents.charAt(tmpIndex - 1))) {
                  tmpIndex--;
                }
                replacement.append(CommentBuilder.from(contents.substring(pipeBefore, tmpIndex)));
                if (paramEnd > tmpIndex) {
                  replacement.append(contents, tmpIndex, paramEnd);
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
          String value = paramInfo.param.getValue();
          boolean empty = (value == null) || value.isEmpty();
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis,
              paramInfo.param.getPipeIndex(), endIndex,
              empty ? ErrorLevel.WARNING : ErrorLevel.CORRECT);
          if (empty) {
            errorResult.addReplacement("");
          }
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
    if (categoryName != null) {
      return categoryName;
    }
    if ((trackingCategory != null) && !trackingCategory.trim().isEmpty()) {
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
    String category = getTrackingCategory();
    if (category != null) {
      API api = APIFactory.getAPI();
      String title = wiki.getWikiConfiguration().getPageTitle(Namespace.CATEGORY, category);
      Page categoryPage = DataManager.createSimplePage(wiki, title, null, null, Namespace.CATEGORY);
      try {
        api.retrieveCategoryMembers(wiki, categoryPage, 0, false, limit);
        result = categoryPage.getRelatedPages(RelatedPages.CATEGORY_MEMBERS);
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
    if (isInWhiteList(analysis.getPage().getTitle())) {
      return analysis.getContents();
    }
    return fixUsingAutomaticReplacement(analysis);
  }

  /**
   * Bean for holding information about a parameter
   */
  private record ParameterInfo(int numParam, Parameter param) {
  }

  /* ====================================================================== */
  /* PARAMETERS                                                             */
  /* ====================================================================== */

  /** Category containing the list of pages in error */
  private static final String PARAMETER_CATEGORY = "category";

  /** Values that can be safely ignored */
  private static final String PARAMETER_IGNORE = "ignore";

  /** Values that are to be considered identical */
  private static final String PARAMETER_IDENTICAL = "identical";

  /**
   * Initialize settings for the algorithm.
   * 
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#initializeSettings()
   */
  @Override
  protected void initializeSettings() {
    String tmp = getSpecificProperty(PARAMETER_IGNORE, true, true, false);
    ignore.clear();
    if (tmp != null) {
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        ignore.addAll(tmpList);
      }
    }
    tmp = getSpecificProperty(PARAMETER_IDENTICAL, true, true, false);
    identicalParameters.clear();
    if (tmp != null) {
      TemplateConfigurationGroup group = new TemplateConfigurationGroup();
      List<String[]> generalList = getWPCConfiguration().getStringArrayList(WPCConfigurationStringList.TEMPLATE_GROUPS);
      if (generalList != null) {
        group.addGroups(generalList);
      }
      List<String[]> tmpList = WPCConfiguration.convertPropertyToStringArrayList(tmp);
      if (tmpList != null) {
        for (String[] params : tmpList) {
          if (params.length > 3) {
            for (String templateName : group.getTemplateNames(params[0])) {
              for (String paramName : params[1].split(",")) {
                identicalParameters.add(new IdenticalParameters(templateName, paramName, Arrays.stream(params).skip(2).collect(Collectors.toSet())));
              }
            }
          }
        }
      }
    }
    tmp = getSpecificProperty(PARAMETER_CATEGORY, true, true, false);
    categoryName = null;
    if ((tmp != null) && !tmp.trim().isEmpty()) {
      categoryName = tmp.trim();
    }
  }

  /** Category containing the list of pages in error */
  private String categoryName = null;

  /** Values that can be safely ignored */
  private final List<String[]> ignore = new ArrayList<>();

  private record IdenticalParameters(String templateName, String parameterName, Set<String> values) {}

  /** Values that are to be considered identical */
  private final List<IdenticalParameters> identicalParameters = new ArrayList<>();

  /**
   * Build the list of parameters for this algorithm.
   */
  @Override
  protected void addParameters() {
    super.addParameters();
    addParameter(new AlgorithmParameter(
        PARAMETER_CATEGORY,
        GT._T("A category containing the list of pages in error"),
        new AlgorithmParameterElement(
            "category name",
            GT._T("A category containing the list of pages in error"))));
    addParameter(new AlgorithmParameter(
        PARAMETER_IDENTICAL,
        GT._T("Values that are to be considered identical"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template names",
                GT._T("Comma separated list of template names")
            ),
            new AlgorithmParameterElement(
                "parameter names",
                GT._T("Comma separated list of parameter names")
            ),
            new AlgorithmParameterElement(
                "parameter value",
                GT._T("Parameter value"),
                false,
                true
            )
        },
        true
    ));
    addParameter(new AlgorithmParameter(
        PARAMETER_IGNORE,
        GT._T("Values that can be safely ignored for a given template and argument"),
        new AlgorithmParameterElement[] {
            new AlgorithmParameterElement(
                "template name",
                GT._T("Name of the template in which some values can be safely ignored")),
            new AlgorithmParameterElement(
                "parameter name",
                GT._T("Name of the parameter for which some values can be safely ignored")),
            new AlgorithmParameterElement(
                "value",
                GT._T("Value of the parameter which can be safely ignored"),
                false,
                true),
        },
        true));
  }
}
