/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.configuration.WPCConfigurationString;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.contents.template.TemplateBuilder;
import org.wikipediacleaner.i18n.GT;


/**
 * A class for describing a matcher for template creating internal links directly from parameter value.
 * {{templateName|parameterName=parameterValue}} gives [[parameterValue]]
 */
public class TemplateMatcher1LT extends TemplateMatcher {

  private final String parameterName;
  private final String parameterDefaultValue;
  private final String neededParameter;

  /**
   * @param wikipedia Wikipedia.
   * @param templateName Template name.
   * @param explanation Explanation.
   * @param isGood Is good ?
   * @param helpNeeded Is help needed ?
   * @param parameterName Parameter name.
   * @param parameterDefaultValue Parameter default value.
   * @param neededParameter Parameter eventually required.
   */
  public TemplateMatcher1LT(
      EnumWikipedia wikipedia, String templateName,
      String explanation, boolean isGood, boolean helpNeeded,
      String parameterName, String parameterDefaultValue,
      String neededParameter) {
    super(wikipedia, templateName, explanation, isGood, helpNeeded);
    this.parameterName = parameterName;
    this.parameterDefaultValue = parameterDefaultValue;
    this.neededParameter = neededParameter;
  }

  /**
   * @param page Page.
   * @param template Template being analyzed.
   * @return Link (if any) created by the template for this matcher.
   */
  @Override
  public String linksTo(Page page, PageElementTemplate template) {
    if ((template == null) || (parameterName == null)) {
      return null;
    }
    if (neededParameter != null) {
      String parameter = template.getParameterValue(neededParameter);
      if ((parameter == null) || (parameter.length() == 0)) {
        return null;
      }
    }
    return getParameterValue(page, template);
  }

  /**
   * @param page Page.
   * @param template Template.
   * @return List of possible kinds of replacements.
   */
  @Override
  public List<String> getReplacements(Page page, PageElementTemplate template) {
    List<String> replacements = new ArrayList<>();
    if (!isGood()) {
      replacements.add(GT._T(
          "Replace parameter {0} with {1}",
          new Object[] {
              parameterName,
              "???" } ));
      String value = getParameterValue(page, template);
      if ((value != null) && (value.trim().length() > 0)) {
        String pipeTemplate = getWikipedia().getConfiguration().getString(WPCConfigurationString.PIPE_TEMPLATE);
        replacements.add(GT._T(
            "Replace parameter {0} with {1}",
            new Object[] {
                parameterName,
                "???{{" + pipeTemplate + "}}" + value} ));
      }
    }
    return replacements;
  }

  /**
   * @param template Template.
   * @param index Replacement index.
   * @param text Replacement text.
   * @return Full replacement.
   */
  @Override
  public String getReplacement(
      Page page, PageElementTemplate template,
      int index, String text) {
    String parameterValue = null;
    switch (index) {
    case 0:
      parameterValue = text;
      break;
    case 1:
      parameterValue =
        text +
        TemplateBuilder.from(getWikipedia().getConfiguration().getString(WPCConfigurationString.PIPE_TEMPLATE)).toString() +
        getParameterValue(page, template);
      break;
    }
    if (parameterValue == null) {
      return null;
    }
    return template.getParameterReplacement(parameterName, parameterValue, neededParameter);
  }

  /**
   * @param page Page.
   * @param template Template
   * @return Parameter value.
   */
  private String getParameterValue(Page page, PageElementTemplate template) {
    String value = template.getParameterValue(parameterName);
    if (value == null) {
      value = parameterDefaultValue;
    }
    return PageContents.expandText(page, value);
  }
}
